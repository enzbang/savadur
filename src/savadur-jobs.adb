------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                      Pascal Obry - Olivier Ramonat                       --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Sets;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Savadur.Actions;
with Savadur.Build;
with Savadur.Config.Environment_Variables;
with Savadur.Environment_Variables;
with Savadur.SCM;
with Savadur.Logs;
with Savadur.Projects;
with Savadur.Remote_Files;
with Savadur.Scenarios;
with Savadur.Utils;

package body Savadur.Jobs is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type Job_Data is record
      Project  : aliased Signed_Files.Handler;
      Scenario : Unbounded_String;
      Time     : Times.Periodic;
   end record;

   End_Job : constant Job_Data :=
               (Project  => <>,
                Scenario => Null_Unbounded_String,
                Time     => Times.No_Time);

   function "<" (J1, J2 : in Job_Data) return Boolean;
   --  Returns the True if J1 must be executed before J2

   package Job_Set is new Containers.Ordered_Sets (Job_Data);
   --  Job ordered on time

   protected Job_Handler is

      procedure Add (Job : in Job_Data);
      --  Add a new job into the queue, jobs are sorted in ascending time

      entry Get (Job : in out Job_Data);
      --  Get first job in the queue

      entry Next (Seconds : out Duration);
      --  Returns the time in seconds to the next job

      entry Rescedule;
      --  Trigger a rescedule of the jobs, this is done when a new job
      --  (possibly scheduled before the current one) is added into the queue.

      procedure Stop;
      --  Schedule a terminating job into the queue

   private
      Jobs    : Job_Set.Set;
      Size    : Natural := 0;
      New_Job : Boolean := False;
   end Job_Handler;

   task type Run_Jobs;
   type Run_Jobs_Handler is access Run_Jobs;

   ---------
   -- "<" --
   ---------

   function "<" (J1, J2 : in Job_Data) return Boolean is
   begin
      --  We always want the ending job to come after all non periodic ones
      if J1 = End_Job then
         return False;
      elsif J2 = End_Job then
         return True;
      else
         return Times.Next_Run_In (J1.Time) < Times.Next_Run_In (J2.Time);
      end if;
   end "<";

   ---------
   -- Add --
   ---------

   procedure Add
     (Project  : in Signed_Files.Handler;
      Scenario : in String;
      Time     : in Times.Periodic := Times.No_Time) is
   begin
      Job_Handler.Add
        (Job_Data'(Project  => Project,
                   Scenario => +Scenario,
                   Time     => Time));
   end Add;

   Runner : Run_Jobs_Handler;

   -----------------
   -- Job_Handler --
   -----------------

   protected body Job_Handler is

      ---------
      -- Add --
      ---------

      procedure Add (Job : in Job_Data) is
      begin
         --  Ensure the job task is created
         if Runner = null then
            Runner := new Run_Jobs;
         end if;

         Jobs.Insert (Job);
         Size := Size + 1;
         New_Job := True;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Job : in out Job_Data) when Size > 0 is
      begin
         Job := Jobs.First_Element;
         Jobs.Delete_First;
         New_Job := False;
      end Get;

      ----------
      -- Next --
      ----------

      entry Next (Seconds : out Duration) when Size > 0 is
      begin
         Seconds := Times.Next_Run_In (Jobs.First_Element.Time);
         New_Job := False;
      end Next;

      ---------------
      -- Rescedule --
      ---------------

      entry Rescedule when New_Job is
      begin
         New_Job := False;
      end Rescedule;

      ----------
      -- Stop --
      ----------

      procedure Stop is
      begin
         if Runner /= null then
            Add (End_Job);
         end if;
      end Stop;

   end Job_Handler;

   --------------
   -- Run_Jobs --
   --------------

   task body Run_Jobs is
      use Projects.Id_Utils;
      use SCM.Id_Utils;
      use type Times.Periodic;

      Project : aliased Projects.Project_Config;
      Job     : Job_Data;
      Env_Var : Environment_Variables.Maps.Map;
      SCM     : Savadur.SCM.SCM;
      pragma Unreferenced (SCM);
      Seconds : Duration;
   begin
      Jobs_Loop : loop

         Wait_For_Job : loop
            Job_Handler.Next (Seconds);

            select
               Job_Handler.Rescedule;
            or
               delay Seconds;
               exit Wait_For_Job;
            end select;
         end loop Wait_For_Job;

         Job_Handler.Get (Job);

         if Job.Time /= Times.No_Time then
            --  This is a periodic job, reschedule it
            Job_Handler.Add (Job);
         end if;

         exit Jobs_Loop when Job = End_Job;

         begin
            Logs.Write
              ("Run : " & (-Job.Scenario) & ", "
               & Signed_Files.Name (Job.Project));

            --  Look for project file

            Project := Remote_Files.Load_Project
              (Signed_Files.Name (Job.Project));

            Logs.Write ("Project Id : " & (-Project.Project_Id));

            Logs.Write
              (Content => "SCM Id     : " & (-Project.SCM_Id),
               Kind    => Logs.Very_Verbose);

            Logs.Write
              (Content => "Action list : " & ASCII.LF
                             & Actions.Image (Project.Actions) & ASCII.LF,
               Kind    => Logs.Very_Verbose);

            Logs.Write
              (Content => "Scenarios : " & ASCII.LF
                            & Scenarios.Image (Project.Scenarios) & ASCII.LF,
               Kind    => Logs.Very_Verbose);

            --  Check if we know about this SCM

            SCM := Remote_Files.Load_SCM (-Project.SCM_Id);

            --  We can run the job

            Env_Var :=
              Savadur.Config.Environment_Variables.Parse (Project'Access);

            if Savadur.Build.Run
              (Project => Project'Access,
               Env_Var => Env_Var,
               Id      => Scenarios.Id (Job.Scenario))
            then
               Logs.Write ("Success");
            else
               Logs.Write ("Failure");
            end if;

         exception
            when E : others =>
               Logs.Write ("ERROR: " & Exception_Information (E));
         end;
      end loop Jobs_Loop;
   end Run_Jobs;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Job_Handler.Stop;
   end Stop;

end Savadur.Jobs;
