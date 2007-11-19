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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Unbounded;

with Savadur.Actions;
with Savadur.Utils;

package Savadur.Scenarios is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   type Id is new Unbounded_String;

   package Id_Utils is new Generic_Utils (Source => Id);

   Default_Scenario : constant Id;

   type Scenario is record
      Id      : Scenarios.Id;
      Actions : Savadur.Actions.Vectors.Vector;
   end record;

   Null_Scenario : constant Scenario;

   function Image (Scenario : in Scenarios.Scenario) return String;
   --  Return Scenario image

   ----------
   -- Sets --
   ----------

   function Hash (Key : in Scenario) return Containers.Hash_Type;
   --  Uses scenario.Id hash (Strings.Hash_Case_Insensitive)

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Scenario,
      Hash                => Hash,
      Equivalent_Elements => "=");

   function Key (Element : in Scenario) return Id;
   --  Returns scenario id

   function Hash (Key : in Id) return Containers.Hash_Type;

   package Keys is new Sets.Generic_Keys
     (Key_Type        => Id,
      Key             => Key,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Scenarios : in Sets.Set) return String;
   --  Returns Scenario map image

private

   Null_Scenario    : constant Scenario :=
                        Scenario'(Id      => Id_Utils.Nil,
                                  Actions => Actions.Vectors.Empty_Vector);
   Default_Scenario : constant Id := Id (+"default");

end Savadur.Scenarios;
