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

with Ada.Strings.Unbounded.Hash;

package body Savadur.Projects.Sets is

   ---------
   -- "=" --
   ---------

   function "=" (P1, P2 : in Project_Config) return Boolean is
   begin
      return P1.Project_Id = P2.Project_Id;
   end "=";

   ----------
   -- Hash --
   ----------

   function Hash (Project : in Project_Config) return Containers.Hash_Type is
   begin
      return Hash (Unbounded_String (Project.Project_Id));
   end Hash;

   ---------
   -- Key --
   ---------

   function Key (Project : in Project_Config) return String is
      use Projects.Id_Utils;
   begin
      return -Project.Project_Id;
   end Key;

end Savadur.Projects.Sets;
