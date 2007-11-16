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
with Ada.Strings.Unbounded;

with Savadur.Config.Project;
with Savadur.Config.Server;
with Savadur.Logs;
with Savadur.Servers;
with Savadur.Client_Service.Client;
with Savadur.Web_Services.Client;
with Savadur.Utils;

package body Savadur.Remote_Files is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Project : in String;
      SHA1    : in Signed_Files.Signature) return Projects.Project_Config
   is
      use type Signed_Files.Signature;

      procedure Download (Cursor : in Servers.Sets.Cursor);
      --  Try downloading Project from this server

      Project_Filename : constant String :=
                           Directories.Compose
                             (Config.Project_File_Directory, Project & ".xml");

      Done             : Boolean := False;
      Found            : Boolean := False;

      --------------
      -- Download --
      --------------

      procedure Download (Cursor : in Servers.Sets.Cursor) is
         Server  : constant Servers.Server := Servers.Sets.Element (Cursor);
         Content : Unbounded_String;
      begin
         if not Done then
            Logs.Write
              ("Try loading " & Project_Filename & " from " & (-Server.URL));
            Content := +Client_Service.Client.Load_Project
              (Project,
               String (SHA1), -Server.URL);

            if Content /= Null_Unbounded_String then
               Logs.Write ("   found.");
               Utils.Set_Content (Project_Filename, -Content);
               Done := True;
            end if;
         end if;
      end Download;

   begin
      Logs.Write ("Load_Project " & Project_Filename);
      if Directories.Exists (Project_Filename) then
         Logs.Write ("   file exists");
         declare
            S_File : Signed_Files.Handler;
         begin
            Signed_Files.Create (S_File, Project_Filename);

            if Signed_Files.SHA1 (S_File) = SHA1 then
               Logs.Write ("   is already up-to-date");
               Found := True;
            else
               Logs.Write ("   is not up-to-date");
            end if;
         end;
      end if;

      if not Found then
         --  Try to download it from known servers
         Config.Server.Configurations.Iterate (Download'Access);
      end if;

      if Found then
         return Config.Project.Parse (Project, Project_Filename);
      else
         raise Unknown_File with "Cannot found project " & Project;
      end if;
   end Load_Project;

end Savadur.Remote_Files;
