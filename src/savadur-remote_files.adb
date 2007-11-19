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

with Ada.Strings.Unbounded;

with Savadur.Config.Project;
with Savadur.Config.Server;
with Savadur.Logs;
with Savadur.Projects.Sets;
with Savadur.Servers;
with Savadur.Client_Service.Client;
with Savadur.Signed_Files;
with Savadur.Utils;

package body Savadur.Remote_Files is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Signed_Project : in Web_Services.Client.Signed_Project)
      return Projects.Project_Config
   is
      use type Signed_Files.Signature;

      procedure Download (Cursor : in Servers.Sets.Cursor);
      --  Tries downloading Project from this server

      Project          : aliased Signed_Files.Handler :=
                           Signed_Files.To_Handler
                             (Signed_Files.External_Handler (Signed_Project));
      Project_Name     : constant String := Signed_Files.Name (Project);

      Done             : Boolean := False;
      Found            : Boolean := False;

      --------------
      -- Download --
      --------------

      procedure Download (Cursor : in Servers.Sets.Cursor) is
         use type Web_Services.Client.Project_Data;
         Server : constant Servers.Server := Servers.Sets.Element (Cursor);
         Data   : Web_Services.Client.Project_Data;
      begin
         if not Done then
            Logs.Write
              ("Try loading " & Project_Name & " from " & (-Server.URL));
            Data := Client_Service.Client.Load_Project
              (Signed_Project, -Server.URL);

            if Data /= Web_Services.Client.No_Data then
               Logs.Write ("   found.");
               Utils.Set_Content (-Data.Filename, -Data.Content);
               Config.Project.Reload (Project_Name);
               Done := True;
            end if;
         end if;
      end Download;

   begin
      Logs.Write ("Load_Project " & Project_Name);

      if Projects.Sets.Keys.Contains
        (Config.Project.Configurations, Project_Name)
      then
         Logs.Write ("   project exists");
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
         return Config.Project.Get (Project_Name);
      else
         raise Unknown_File with "Cannot found project " & Project_Name;
      end if;
   end Load_Project;

end Savadur.Remote_Files;
