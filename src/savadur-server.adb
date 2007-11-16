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

with Ada.Directories;

with Savadur.Config;
with Savadur.Logs;
with Savadur.Server_Service.Client;
with Savadur.Signed_Files;
with Savadur.Web.Server;

procedure Savadur.Server is
   use Ada;
begin
   Web.Server.Start;

   delay 5.0;

   Logs.Write ("Call a client to run a project");

   declare
      Project          : constant String := "style_checker";
      Project_Filename : constant String :=
                           Directories.Compose
                             (Config.Project_File_Directory, Project & ".xml");
      S_File           : Signed_Files.Handler;
   begin
      Signed_Files.Create (S_File, Project_Filename);

      Savadur.Server_Service.Client.Run
        ("nightly",
         Project,
         String (Signed_Files.SHA1 (S_File)));
   end;
end Savadur.Server;
