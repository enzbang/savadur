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

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Hash_Case_Insensitive;

package Savadur.Projects.Sets is

   use Ada;

   function Hash (Project : in Project_Config) return Containers.Hash_Type;
   --  Uses Project_Config.Project_Id hash (Strings.Hash_Case_Insensitive)

   function "=" (P1, P2 : in Project_Config) return Boolean;

   package Sets is new Containers.Hashed_Sets
     (Element_Type        => Project_Config,
      Hash                => Hash,
      Equivalent_Elements => "=");

   subtype Set is Sets.Set;

   function Key (Project : in Project_Config) return String;

   package Keys is new Sets.Generic_Keys
     (String, Key, Strings.Hash_Case_Insensitive, "=");

end Savadur.Projects.Sets;
