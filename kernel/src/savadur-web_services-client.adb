------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                        Copyright (C) 2007-2008                           --
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

with Ada.Directories;

with Savadur.Clients;
with Savadur.Config.SCM;
with Savadur.Config.Project;
with Savadur.Database;
with Savadur.Logs;
with Savadur.Projects.Sets;
with Savadur.SCM;
with Savadur.Signed_Files;
with Savadur.Utils;

package body Savadur.Web_Services.Client is

   use Ada;
   use Savadur.Utils;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Signed_Project : in Client.Signed_Project) return File_Data
   is
      use type Signed_Files.Signature;

      Project      : aliased Signed_Files.Handler :=
                       Signed_Files.To_Handler
                         (Signed_Files.External_Handler (Signed_Project));
      Project_Name : constant String := Signed_Files.Name (Project);
   begin
      Logs.Write ("Load_Project " & Project_Name);

      if Projects.Sets.Keys.Contains
        (Config.Project.Configurations, Project_Name)
      then
         declare
            Proj             : aliased Projects.Project_Config :=
                                 Config.Project.Get (Project_Name);
            Project_Filename : constant String :=
                                 Projects.Project_Filename (Proj'Access);
            Local_Project    : aliased Signed_Files.Handler;
         begin
            Signed_Files.Create
              (Local_Project, Project_Name, Project_Filename);

            if Signed_Files.SHA1 (Local_Project) =
              Signed_Files.SHA1 (Project)
            then
               Logs.Write ("   up-to-date");
               --  Client up-to-date
               return No_File;

            else
               Logs.Write ("   content returned");
               return File_Data'
                 (Filename => +Directories.Simple_Name (Project_Filename),
                  Content  => +Utils.Content (Project_Filename));
            end if;
         end;

      else
         Logs.Write ("   project not found!");
         return No_File;
      end if;
   end Load_Project;

   --------------
   -- Load_SCM --
   --------------

   function Load_SCM (Signed_SCM : in Client.Signed_SCM) return File_Data is
      use SCM.Id_Utils;
      use type Signed_Files.Signature;

      S_SCM    : aliased Signed_Files.Handler :=
                   Signed_Files.To_Handler
                     (Signed_Files.External_Handler (Signed_SCM));
      SCM_Name : constant String := Signed_Files.Name (S_SCM);
   begin
      Logs.Write ("Load_SCM " & SCM_Name);

      if SCM.Keys.Contains
        (Config.SCM.Configurations, Value (SCM_Name))
      then
         declare
            S            : aliased SCM.SCM := Config.SCM.Get (SCM_Name);
            SCM_Filename : constant String := -S.Filename;
            Local_SCM    : aliased Signed_Files.Handler;
         begin
            Signed_Files.Create
              (Local_SCM, SCM_Name, SCM_Filename);

            if Signed_Files.SHA1 (Local_SCM) = Signed_Files.SHA1 (S_SCM) then
               Logs.Write ("   up-to-date");
               --  Client up-to-date
               return No_File;

            else
               Logs.Write ("   content returned");
               return File_Data'
                 (Filename => +Directories.Simple_Name (SCM_Filename),
                  Content  => +Utils.Content (SCM_Filename));
            end if;
         end;

      else
         Logs.Write ("   SCM not found!");
         return No_File;
      end if;
   end Load_SCM;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key               : in String;
      Data              : in Metadata;
      Server_Name       : in String;
      Callback_Endpoint : in String) is
   begin
      Logs.Write ("Register new client : " & Key & ":"
                    & Server_Name & '@' & Callback_Endpoint);
      Clients.Registered.Insert
        (New_Item => (+Key, Data, +Server_Name, +Callback_Endpoint));
      Database.Login (Key);
   end Register;

   ------------
   -- Status --
   ------------

   procedure Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Output       : in String;
      Result       : in Boolean;
      Job_Id       : in Natural)
   is
   begin
      Logs.Write (Key & ":" & Project_Name);
      Logs.Write ("Running Job Id" & Natural'Image (Job_Id)
                    & " :: " & Scenario & "/" & Action);
      Logs.Write ("Output is " & Output);
      Logs.Write (Boolean'Image (Result));

      if Action /= "" then
         --  This is the action log. Scenario is in progress
         Database.Log (Key, Project_Name, Scenario,
                       Action, Output, Result, Job_Id);

      else
         --  End of scenario. Final status
         Database.Final_Status (Key, Project_Name, Scenario, Result, Job_Id);
      end if;
   end Status;

end Savadur.Web_Services.Client;
