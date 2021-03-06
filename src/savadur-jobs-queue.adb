------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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

with Ada.Calendar;
with Ada.Containers.Ordered_Sets;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with AWS.Utils;

with Savadur.Actions;
with Savadur.Config.Environment_Variables;
with Savadur.Config.Project;
with Savadur.Logs;
with Savadur.Projects.Sets;
with Savadur.Remote_Files;
with Savadur.SCM;
with Savadur.Utils;

package body Savadur.Jobs.Queue is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type Job_Data is record
      Project        : aliased Signed_Files.Handler;
      Patch_Filename : Unbounded_String;
      Server         : Unbounded_String;
      Scenario       : Unbounded_String;
      Time           : Times.Periodic;  -- periodic job
      Start          : Duration;        -- non periodic job start delay
      Created        : Calendar.Time;
      Number         : Integer;
      Id             : Natural;
   end record;

   End_Job : constant Job_Data :=
               Job_Data'(Project        => <>,
                         Patch_Filename => Null_Unbounded_String,
                         Scenario       => Null_Unbounded_String,
                         Server         => Null_Unbounded_String,
                         Time           => Times.No_Time,
                         Start          => 0.0,
                         Created        => <>,
                         Number         => Integer'First,
                         Id             => 0);

   function "=" (J1, J2 : in Job_Data) return Boolean;
   --  Returns True and J1 and J2 are equivalent jobs

   function "<" (J1, J2 : in Job_Data) return Boolean;
   --  Returns the True if J1 must be executed before J2

   package Job_Set is new Containers.Ordered_Sets (Element_Type => Job_Data);
   --  Job ordered on time

   function Image (Job : in Job_Data) return String;
   --  Returns Job_Data string representation

   function Run_In (Job : in Job_Data) return Duration;
   --  Returns the number of second before the job starts

   protected Job_Handler is

      procedure Add (Job : in Job_Data);
      --  Adds a new job into the queue, jobs are sorted in ascending time

      entry Get (Job : in out Job_Data);
      --  Gets first job in the queue

      entry Next (Seconds : out Duration);
      --  Returns the time in seconds to the next job

      entry Rescedule;
      --  Trigger a rescedule of the jobs, this is done when a new job
      --  (possibly scheduled before the current one) is added into the queue.

      procedure Stop;
      --  Schedule a terminating job into the queue

      function Next_Job_In
        (Project : in Signed_Files.Handler) return Duration;
      --  Returns seconds to next job for the given project

   private
      Jobs    : Job_Set.Set;
      Size    : Natural := 0;
      New_Job : Boolean := False;
      Index   : Natural := 0;
   end Job_Handler;

   task type Run_Jobs;
   type Run_Jobs_Handler is access Run_Jobs;

   Runner : Run_Jobs_Handler;

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
      elsif Run_In (J1) = Run_In (J2) then
         return J1.Number < J2.Number;
      else
         return Run_In (J1) < Run_In (J2);
      end if;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (J1, J2 : in Job_Data) return Boolean is
   begin
      return J1.Number = J2.Number;
   end "=";

   ---------
   -- Add --
   ---------

   procedure Add
     (Project  : in Signed_Files.Handler;
      Patch    : in String;
      Server   : in String;
      Scenario : in String;
      Time     : in Times.Periodic := Times.No_Time;
      Latency  : in Duration := 1.0;
      Id       : in Natural := 0) is
   begin
      Job_Handler.Add
        (Job_Data'(Project        => Project,
                   Patch_Filename => +Patch,
                   Scenario       => +Scenario,
                   Server         => +Server,
                   Time           => Time,
                   Start          => Latency,
                   Created        => Calendar.Clock,
                   Number         => 0,
                   Id             => Id));
   end Add;

   ---------------------------
   -- Add_Periodic_Scenario --
   ---------------------------

   procedure Add_Periodic_Scenario is

      procedure Handle_Project (Position : in Projects.Sets.Sets.Cursor);
      --  Looks for periodic scenario into the project

      Project : Projects.Project_Config;

      --------------------
      -- Handle_Project --
      --------------------

      procedure Handle_Project (Position : in Projects.Sets.Sets.Cursor) is

         procedure Handle_Scenario (Position : in Scenarios.Sets.Cursor);
         --  Schedules periodic scenario

         ---------------------
         -- Handle_Scenario --
         ---------------------

         procedure Handle_Scenario (Position : in Scenarios.Sets.Cursor) is
            use Scenarios.Id_Utils;
            use type Times.Periodic;
            Scenario : constant Scenarios.Scenario :=
                         Scenarios.Sets.Element (Position);
         begin
            if Scenario.Periodic /= Times.No_Time then
               --  A periodic scenario can not use Patch feature
               Add (Project  => Project.Signature,
                    Patch    => "",
                    Server   => "",
                    Scenario => -Scenario.Id,
                    Time     => Scenario.Periodic);
            end if;
         end Handle_Scenario;

      begin
         Project := Projects.Sets.Sets.Element (Position);
         Project.Scenarios.Iterate (Handle_Scenario'Access);
      end Handle_Project;

   begin
      Config.Project.Configurations.Iterate (Handle_Project'Access);
   end Add_Periodic_Scenario;

   -----------
   -- Image --
   -----------

   function Image (Job : in Job_Data) return String is
      use type Times.Periodic;
      Res : Unbounded_String;
   begin
      if Job.Time = Times.No_Time then
         Res := +"(start in " & AWS.Utils.Significant_Image (Job.Start, 2);
      else
         Res := +"(periodic";
      end if;

      Res := Res & " | server:" & To_String (Job.Server)
        & ", scenario:" & To_String (Job.Scenario)
        & ", project:"
        & String (Signed_Files.To_External_Handler (Job.Project))
        & ")";

      return To_String (Res);
   end Image;

   -----------------
   -- Job_Handler --
   -----------------

   protected body Job_Handler is

      ---------
      -- Add --
      ---------

      procedure Add (Job : in Job_Data) is
         use type Times.Periodic;
         use type Job_Set.Cursor;
         use type Signed_Files.Handler;

         Position  : Job_Set.Cursor;
         Local_Job : Job_Data := Job;
      begin
         --  Ensure the job task is created
         Logs.Write
           ("Add job protected "  & Image (Job),
            Kind => Logs.Handler.Very_Verbose);

         if Runner = null then
            Logs.Write
              ("Start Job runner tasks",
               Kind => Logs.Handler.Very_Verbose);
            Runner := new Run_Jobs;
         end if;

         if Local_Job /= End_Job then
            Index := Index + 1;
            Local_Job.Number := Index;
         end if;

         --  If a periodic job, replace it if it already exists. This can
         --  happen when reloading projects. We want to replace the existing
         --  job as the period could have been changed.

         if Local_Job.Time = Times.No_Time then

            --  Remove current non periodic job that could have already been
            --  sceduled.

            Delete_Non_Periodic_Jobs : declare
               Pos : Job_Set.Cursor := Jobs.First;
            begin
               Delete_Current_Job : while Pos /= Job_Set.No_Element loop
                  declare
                     J : constant Job_Data := Job_Set.Element (Pos);
                  begin
                     if J.Project = Job.Project
                       and then J.Server = Job.Server
                       and then J.Scenario = Job.Scenario
                       and then J.Time = Times.No_Time
                     then
                        exit Delete_Current_Job;
                     end if;
                  end;
                  Pos := Job_Set.Next (Pos);
               end loop Delete_Current_Job;

               if Pos = Job_Set.No_Element then
                  Size := Size + 1;

               else
                  Logs.Write
                    ("Delete already scheduled job for this scenario: "
                     & Image (Job_Set.Element (Pos)),
                     Kind => Logs.Handler.Very_Verbose);
                  Jobs.Delete (Pos);
               end if;

               Jobs.Insert (Local_Job);
            end Delete_Non_Periodic_Jobs;

         else
            --  This is periodic job, replace existing one if present

            Position := Jobs.Find (Local_Job);

            if Position /= Job_Set.No_Element then
               Jobs.Replace_Element (Position, Local_Job);
            else
               Jobs.Insert (Local_Job);
               Size := Size + 1;
            end if;
         end if;

         New_Job := True;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Job : in out Job_Data) when Size > 0 is
      begin
         Job := Jobs.First_Element;
         Jobs.Delete_First;
         Size := Size - 1;
         Logs.Write
           ("Pop to Job " & Image (Job),
            Kind => Logs.Handler.Very_Verbose);
      end Get;

      ----------
      -- Next --
      ----------

      entry Next (Seconds : out Duration) when Size > 0 is
         Job : constant Job_Data := Jobs.First_Element;
      begin
         Seconds := Run_In (Job);
         New_Job := False;
      end Next;

      -----------------
      -- Next_Job_In --
      -----------------

      function Next_Job_In
        (Project : in Signed_Files.Handler) return Duration
      is
         use type Signed_Files.Handler;
         Position : Job_Set.Cursor := Jobs.First;
      begin
         while Job_Set.Has_Element (Position) loop
            declare
               Job : constant Job_Data := Job_Set.Element (Position);
            begin
               if Job.Project = Project then
                  return Run_In (Job);
               end if;
            end;

            Job_Set.Next (Position);
         end loop;

         return 0.0;
      end Next_Job_In;

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

   -----------------
   -- Next_Job_In --
   -----------------

   function Next_Job_In (Project : in Signed_Files.Handler) return Duration is
   begin
      return Job_Handler.Next_Job_In (Project);
   end Next_Job_In;

   ------------
   -- Run_In --
   ------------

   function Run_In (Job : in Job_Data) return Duration is
      use type Times.Periodic;
   begin
      if Job.Time = Times.No_Time then
         return Job.Start;
      else
         return Times.Next_Run_In (Job.Time);
      end if;
   end Run_In;

   --------------
   -- Run_Jobs --
   --------------

   task body Run_Jobs is
      use Projects.Id_Utils;
      use SCM.Id_Utils;
      use type Times.Periodic;
      use type Scenarios.Run_Status;
      use type SCM.Id;

      Project : aliased Projects.Project_Config;
      Job     : Job_Data;
      Env_Var : Environment_Variables.Containers.Maps.Map;
      Seconds : Duration;
   begin
      Jobs_Loop : loop

         Wait_For_Job : loop
            Job_Handler.Next (Seconds);
            Logs.Write
              ("Next job in " & Duration'Image (Seconds),
               Kind => Logs.Handler.Very_Verbose);

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

         Run_Job : begin
            Logs.Write
              (Natural'Image (Job.Number)
               & ") Run : " & (-Job.Scenario) & ", "
               & Natural'Image (Job.Id) & ", "
               & Signed_Files.Name (Job.Project));

            --  Look for project file

            Project := Remote_Files.Load_Project
              (Signed_Files.Name (Job.Project));

            --  Look for patch if exists

            if Job.Patch_Filename /= Null_Unbounded_String then
               Remote_Files.Load_Patch
                 (-Job.Patch_Filename,
                  -Job.Server);
            end if;

            Logs.Write ("Project Id : " & (-Project.Project_Id));

            Logs.Write
              (Content => "SCM Id     : " & (-Project.SCM_Id),
               Kind    => Logs.Handler.Very_Verbose);

            Logs.Write
              (Content => "Action list : " & ASCII.LF
                             & Actions.Image (Project.Actions) & ASCII.LF,
               Kind    => Logs.Handler.Very_Verbose);

            Logs.Write
              (Content => "Scenarios : " & ASCII.LF
                            & Scenarios.Image (Project.Scenarios) & ASCII.LF,
               Kind    => Logs.Handler.Very_Verbose);

            --  Check if we know about this SCM

            if Project.SCM_Id /= SCM.Null_Id then
               Load_SCM : declare
                  SCM : Savadur.SCM.SCM;
                  pragma Unreferenced (SCM);
               begin
                  SCM := Remote_Files.Load_SCM (-Project.SCM_Id);
               end Load_SCM;
            end if;

            --  We can run the job

            Env_Var :=
              Savadur.Config.Environment_Variables.Parse (Project'Access);

            if Run
              (Project  => Project'Access,
               Patch    => -Job.Patch_Filename,
               Server   => -Job.Server,
               Env_Var  => Env_Var,
               Scenario => Scenarios.Id (Job.Scenario),
               Id       => Job.Id) = Scenarios.Success
            then
               Logs.Write ("Success");
            else
               Logs.Write ("Failure");
            end if;

         exception
            when E : others =>
               Logs.Write ("ERROR: " & Exception_Information (E));
         end Run_Job;
      end loop Jobs_Loop;
   end Run_Jobs;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Job_Handler.Stop;
   end Stop;

end Savadur.Jobs.Queue;
