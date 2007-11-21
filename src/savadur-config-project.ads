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

with Savadur.Projects.Sets;

package Savadur.Config.Project is

   Config_Error : exception;

   Configurations : Savadur.Projects.Sets.Set;

   procedure Parse;
   --  Parses all projects

   function Get (Project_Name : in String) return Projects.Project_Config;
   --  Returns the project with the given name

   procedure Reload (Project_Name : in String);
   --  Reloads project name

   function Parse (Filename : in String) return Projects.Project_Config;
   --  Returns the project configuration read in the given file, add it to the
   --  sets of known projects.

end Savadur.Config.Project;
