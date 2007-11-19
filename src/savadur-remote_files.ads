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

with Savadur.SCM;
with Savadur.Projects;

package Savadur.Remote_Files is

   Unknown_File : exception;

   function Load_Project
     (Project_Name : in String) return Projects.Project_Config;
   --  Returns the project, try to download it from all known server if not
   --  found locally. Raises Unknown_File if the file cannot be found.

   function Load_SCM (SCM_Name : in String) return SCM.SCM;
   --  Returns the SCM, try to download it from all known server if not
   --  found locally. Raises Unknown_File if the file cannot be found.

end Savadur.Remote_Files;
