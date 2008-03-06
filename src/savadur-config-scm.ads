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

with Savadur.SCM;

package Savadur.Config.SCM is

   Config_Error : exception renames Savadur.Config.Config_Error;

   Configurations : Savadur.SCM.Sets.Set;

   procedure Parse;
   --  Fills the SCM configuration map

   function Get (SCM_Name : in String) return Savadur.SCM.SCM;
   --  Returns the SCM with the given name

   procedure Reload (SCM_Name : in String; Filename : in String := "");
   --  Reloads SCM name or load the given filename if no SCM found

end Savadur.Config.SCM;
