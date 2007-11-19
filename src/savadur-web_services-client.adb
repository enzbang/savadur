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

--  with Ada.Directories;
with Ada.Text_IO;

with Savadur.Clients;
with Savadur.Config.Project;
with Savadur.Logs;
with Savadur.Projects.Sets;
with Savadur.Signed_Files;
with Savadur.Utils;

package body Savadur.Web_Services.Client is

   use Ada;
   use Savadur.Utils;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Signed_Project : in Client.Signed_Project) return Project_Data
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

            if Signed_Files.SHA1 (Local_Project'Access) =
              Signed_Files.SHA1 (Project'Access)
            then
               Logs.Write ("   up-to-date");
               --  Client up-to-date
               return No_Data;

            else
               Logs.Write ("   content returned");
               return Project_Data'
                 (Filename => +Project_Filename,
                  Content  => +Utils.Content (Project_Filename));
            end if;
         end;

      else
         Logs.Write ("   project not found!");
         return No_Data;
      end if;
   end Load_Project;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key               : in String;
      Data              : in Metadata;
      Callback_Endpoint : in String) is
   begin
      Text_IO.Put_Line
        ("Register new client : " & Key & '@' & Callback_Endpoint);
      Clients.Registered.Insert (New_Item => (+Key, Data, +Callback_Endpoint));
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
      Result       : in Returned_Status)
   is
      pragma Unreferenced
        (Key, Project_Name, Scenario, Action, Output, Result);
   begin
      null;
   end Status;

end Savadur.Web_Services.Client;
