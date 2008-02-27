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

with Savadur.Utils;
with Savadur.SCM;
with Savadur.Actions;
with Savadur.Scenarios;
with Savadur.Notifications;
with Savadur.Variables;
with Savadur.Signed_Files;

package Savadur.Projects is

   use Ada.Strings.Unbounded;

   type Project_Id is new Unbounded_String;
   type Project_Description is new Unbounded_String;

   package Id_Utils is new Utils.Generic_Utils (Source => Project_Id);

   package Desc_Utils is
     new Utils.Generic_Utils (Source => Project_Description);

   type Project_Directories is private;

   type Project_Config is record
      Project_Id    : Projects.Project_Id;
      Signature     : Signed_Files.Handler;
      SCM_Id        : Savadur.SCM.Id;
      Actions       : Savadur.Actions.Sets.Set;
      Scenarios     : Savadur.Scenarios.Sets.Set;
      Notifications : Savadur.Notifications.Hooks;
      Variables     : Savadur.Variables.Sets.Set;
      Directory     : Project_Directories;
      Description   : Project_Description;
   end record;

   procedure Set_Filename
     (Project  : access Project_Config;
      Filename : in     String);
   --  Sets project filename

   function Project_Directory
     (Project : access Project_Config) return String;
   --  Returns project directory (or create it if does not exist)

   function Project_Filename
     (Project : access Project_Config) return String;
   --  Returns project filename

   function Project_Env_Filename
     (Project : access Project_Config) return String;
   --  Returns project env filename

   function Project_Log_Directory
     (Project : access Project_Config) return String;
   --  Returns project log directory (or create it if does not exist)

   function Project_State_Directory
     (Project : access Project_Config) return String;
   --  Returns project state directory (or create it if does not exist)

   function Project_Sources_Directory
     (Project : access Project_Config) return String;
   --  Returns project sources directory (do *not* create it if does not exist)

private

   type Project_Directories is record
      Cached_Project_Directory         : access String := null;
      Cached_Project_Env_Filename      : access String := null;
      Cached_Project_Filename          : access String := null;
      Cached_Project_Log_Directory     : access String := null;
      Cached_Project_State_Directory   : access String := null;
      Cached_Project_Sources_Directory : access String := null;
   end record;

end Savadur.Projects;
