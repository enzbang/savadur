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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Savadur.Build;
with Savadur.Config.SCM;
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
      Project  : Unbounded_String;
      Scenario : Unbounded_String;
      SHA1     : Signed_Files.Signature;
   end record;

   End_Job : constant Job_Data :=
               (Null_Unbounded_String,
                Null_Unbounded_String,
                Signed_Files.No_SHA1);

   package Job_List is new Containers.Doubly_Linked_Lists (Job_Data);

   protected Job_Handler is
      procedure Add (Job : in Job_Data);
      entry Get (Job : in out Job_Data);
      procedure Stop;
   private
      Jobs : Job_List.List;
      Size : Natural := 0;
   end Job_Handler;

   task type Run_Jobs;
   type Run_Jobs_Handler is access Run_Jobs;

   ---------
   -- Add --
   ---------

   procedure Add
     (Project_Name : in String;
      Scenario     : in String;
      SHA1         : in Signed_Files.Signature) is
   begin
      Job_Handler.Add
        (Job_Data'(Project  => +Project_Name,
                   Scenario => +Scenario,
                   SHA1     => SHA1));
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

         Jobs.Append (Job);
         Size := Size + 1;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Job : in out Job_Data) when Size > 0 is
      begin
         Job := Jobs.First_Element;
         Jobs.Delete_First;
      end Get;

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

      Job     : Job_Data;
      Project : aliased Projects.Project_Config;
      Env_Var : Environment_Variables.Maps.Map;
      SCM     : Savadur.SCM.SCM;
   begin
      Jobs_Loop : loop
         Job_Handler.Get (Job);

         exit Jobs_Loop when Job = End_Job;

         begin
            Logs.Write ("Run : " & (-Job.Scenario) & ", " & (-Job.Project));

            --  Look for project file

            Project := Remote_Files.Load_Project (-Job.Project, Job.SHA1);

            Logs.Write ("Project Id : " & (-Project.Project_Id));
            Logs.Write ("SCM Id     : " & (-Project.SCM_Id));

            --  Check if we know about this SCM

            SCM.Id := Project.SCM_Id;

            if Config.SCM.Configurations.Contains (SCM) then
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

            else
               --  ??? we have to load it
               Logs.Write ("Need to load the SCM");
               --  ??? TBD
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
