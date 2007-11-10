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

with Savadur.Actions;
with Savadur.Scenarios;
with Savadur.SCM;
with Savadur.Variables;
with Savadur.Utils;

package Savadur.Config.Project is

   use Ada.Strings.Unbounded;

   type Project_Id is new Unbounded_String;

   package Project_Id_Utils is new Savadur.Utils.Generic_Utils (Project_Id);

   type Project_Config is record
      Project_Id : Project.Project_Id;
      SCM_Id     : Savadur.SCM.Id;
      Actions    : Savadur.Actions.Sets.Set;
      Scenarios  : Savadur.Scenarios.Sets.Set;
      Variables  : Savadur.Variables.Sets.Set := Savadur.Variables.Default;
   end record;

   function Parse (Filename : String) return Project_Config;
   --  Returns the project configuration read in the given file

end Savadur.Config.Project;
