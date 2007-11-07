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
with Ada.Containers.Indefinite_Hashed_Maps;

with Savadur.Action;

package Savadur.Scenario is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;

   Default_Scenario : Id := "default";

   type Mode is new Unbounded_String;

   type Scenario is record
      Mode    : Savadur.Scenario.Mode;
      Actions : Savadur.Action.Vectors.Vector;
   end record;

   function Image (Scenario : Savadur.Scenario.Scenario) return String;
   --  Return Scenario image

   function Hash (Key : Id) return Containers.Hash_Type;
   --  Renames Strings.Hash_Case_Insensitive

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => Scenario,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Scenari : Savadur.Scenario.Maps.Map) return String;
   --  Return Scenario map image

end Savadur.Scenario;
