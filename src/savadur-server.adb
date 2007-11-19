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

with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Logs;
with Savadur.Projects;
with Savadur.Server_Service.Client;
with Savadur.Signed_Files;
with Savadur.Web.Server;
with Savadur.Web_Services.Server;

procedure Savadur.Server is
--     use Ada;
begin
   Config.SCM.Parse;
   Config.Project.Parse;

   Web.Server.Start;

   delay 5.0;

   Logs.Write ("Call a client to run a project");

   declare
      Project_Name     : constant String := "style_checker";
      Project          : aliased Projects.Project_Config :=
                           Config.Project.Get (Project_Name);
      Project_Filename : constant String :=
                           Projects.Project_Filename (Project'Access);
      Signed_Project   : aliased Signed_Files.Handler;
   begin
      Signed_Files.Create (Signed_Project, Project_Name, Project_Filename);

      Savadur.Server_Service.Client.Run
        (Scenario       => "nightly",
         Signed_Project =>
           Web_Services.Server.Signed_Project
             (Signed_Files.To_External_Handler (Signed_Project'Access)));
   end;
end Savadur.Server;
