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

with IO_Exceptions;
with Savadur.Projects.Sets;

package Savadur.Config.Project is

   Config_Error : exception renames Savadur.Config.Config_Error;

   Name_Error : exception renames IO_Exceptions.Name_Error;

   Configurations : Savadur.Projects.Sets.Set;

   procedure Parse;
   --  Parses all projects

   function Get (Project_Name : in String) return Projects.Project_Config;
   --  Returns the project with the given name or
   --  raise Name_Error if there is no project with the given name

   function Is_Project_Name (Project_Name : in String) return Boolean;
   --  Returns TRUE if a project with the given name exists

   procedure Reload (Project_Name : in String; Filename : in String := "");
   --  Reloads project name or load the given filename if no project found

   function Parse (Filename : in String) return Projects.Project_Config;
   --  Returns the project configuration read in the given file, add it to the
   --  sets of known projects.

end Savadur.Config.Project;
