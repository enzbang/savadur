------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
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

package Savadur.Projects is

   use Ada.Strings.Unbounded;

   type Project_Id is new Unbounded_String;

   package Project_Id_Utils is new Utils.Generic_Utils (Source => Project_Id);

   type Project_Directories is private;

   type Project_Config is record
      Project_Id    : Projects.Project_Id;
      SCM_Id        : Savadur.SCM.Id;
      Actions       : Savadur.Actions.Sets.Set;
      Scenarios     : Savadur.Scenarios.Sets.Set;
      Notifications : Savadur.Notifications.Hooks;
      Variables     : Savadur.Variables.Sets.Set;
      Directories   : Project_Directories;
   end record;

   procedure Set_Filename
     (Project  : access Project_Config;
      Filename : in     String);
   --  Set project filename

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
      Project_Directory         : Unbounded_String;
      Project_Log_Directory     : Unbounded_String;
      Project_State_Directory   : Unbounded_String;
      Project_Sources_Directory : Unbounded_String;
      Project_Filename          : Unbounded_String;
      Project_Env_Filename      : Unbounded_String;
   end record;

end Savadur.Projects;
