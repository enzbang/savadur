------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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
with Ada.Directories;

with Savadur.Config.SCM;
with Savadur.Config.Project;
with Savadur.Logs;
with Savadur.Projects.Sets;
with Savadur.Servers;
with Savadur.Client_Service.Client;
with Savadur.Signed_Files;
with Savadur.Utils;

with Savadur.Web_Services.Client;

package body Savadur.Remote_Files is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Project_Name : in String) return Projects.Project_Config
   is
      use type Signed_Files.Signature;

      procedure Download (Cursor : in Servers.Sets.Cursor);
      --  Tries downloading Project from this server

      Signed_Project   : aliased Signed_Files.Handler;

      Loaded           : Boolean := False;
      Found            : Boolean := False;

      --------------
      -- Download --
      --------------

      procedure Download (Cursor : in Servers.Sets.Cursor) is
         use type Web_Services.Client.File_Data;
         Server : constant Servers.Server := Servers.Sets.Element (Cursor);
         Data   : Web_Services.Client.File_Data;
      begin
         if not Loaded then
            Logs.Write
              ("Try loading " & Project_Name & " from " & (-Server.URL));

            Data := Client_Service.Client.Load_Project
              (Web_Services.Client.Signed_Project
                 (Signed_Files.To_External_Handler (Signed_Project)),
                  -Server.URL);

            if Data /= Web_Services.Client.No_File then
               Logs.Write ("   found new or updated");
               Data.Filename := +Directories.Compose
                 (Containing_Directory => Config.Project_File_Directory,
                  Name                 => -Data.Filename);

               Utils.Set_Content (-Data.Filename, -Data.Content);
               Config.Project.Reload (Project_Name, -Data.Filename);
               Loaded := True;
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
            Project          : aliased Projects.Project_Config :=
                                 Config.Project.Get (Project_Name);
            Project_Filename : constant String :=
                                 Projects.Project_Filename (Project'Access);
         begin
            Signed_Files.Create
              (Signed_Project, Project_Name, Project_Filename);
            Found := True;
         end;

      else
         Logs.Write ("   project does not exist");
         Signed_Files.Create (Signed_Project, Project_Name, "");
      end if;

      --  Download the project from the servers if not up-to-date

      Servers.Online_Iterate (Download'Access);

      if Found or else Loaded then
         return Config.Project.Get (Project_Name);
      else
         raise Unknown_File with "Cannot found project " & Project_Name;
      end if;
   end Load_Project;

   --------------
   -- Load_SCM --
   --------------

   function Load_SCM (SCM_Name : in String) return SCM.SCM is
      use SCM.Id_Utils;

      procedure Download (Cursor : in Servers.Sets.Cursor);
      --  Tries downloading SCM from this server

      Signed_SCM : aliased Signed_Files.Handler;
      Loaded     : Boolean := False;
      Found      : Boolean := False;

      --------------
      -- Download --
      --------------

      procedure Download (Cursor : in Servers.Sets.Cursor) is
         use type Web_Services.Client.File_Data;
         Server : constant Servers.Server := Servers.Sets.Element (Cursor);
         Data   : Web_Services.Client.File_Data;
      begin
         if not Loaded then
            Logs.Write
              ("Try loading " & SCM_Name & " from " & (-Server.URL));

            Data := Client_Service.Client.Load_SCM
              (Web_Services.Client.Signed_SCM
                 (Signed_Files.To_External_Handler (Signed_SCM)),
                  -Server.URL);

            if Data /= Web_Services.Client.No_File then
               Logs.Write ("   found new or updated");

               Data.Filename := +Directories.Compose
                 (Containing_Directory => Config.SCM_Directory,
                  Name                 => -Data.Filename);

               Utils.Set_Content (-Data.Filename, -Data.Content);
               Config.SCM.Reload (SCM_Name, -Data.Filename);
               Loaded := True;
            end if;
         end if;
      end Download;

   begin
      Logs.Write ("Load_SCM " & SCM_Name);

      if SCM.Keys.Contains (Config.SCM.Configurations, Value (SCM_Name)) then
         Logs.Write ("   SCM exists");
         declare
            S            : aliased SCM.SCM := Config.SCM.Get (SCM_Name);
            SCM_Filename : constant String := -S.Filename;
         begin
            Signed_Files.Create (Signed_SCM, SCM_Name, SCM_Filename);
            Found := True;
         end;

      else
         Logs.Write ("   SCM does not exist");
         Signed_Files.Create (Signed_SCM, SCM_Name, "");
      end if;

      --  Download the project from the servers if not up-to-date

      Servers.Online_Iterate (Download'Access);

      if Found or else Loaded then
         return Config.SCM.Get (SCM_Name);
      else
         raise Unknown_File with "Cannot found project " & SCM_Name;
      end if;
   end Load_SCM;

end Savadur.Remote_Files;
