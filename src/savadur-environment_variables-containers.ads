------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2008                             --
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

with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

package Savadur.Environment_Variables.Containers is

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Savadur.Environment_Variables.Var,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   function Image (M : in Maps.Map) return String;
   --  Returns environment variables image

   procedure Set_Environment (M : in Maps.Map);
   --  Reads environment variable in env/$project_name$.xml
   --  and sets environment variables according to rules

end Savadur.Environment_Variables.Containers;
