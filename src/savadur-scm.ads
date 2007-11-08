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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;

with Savadur.Utils;
with Savadur.Actions;

package Savadur.SCM is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   --  Special actions references

   SCM_Init : Actions.Ref_Action :=
     (Action_Type => Actions.SCM,
      Id          => Actions.U_Id (+"init"));

   type Id is new String;
   type U_Id is new Unbounded_String;

   function "+" (Source : Id) return U_Id;

   function "-" (Source : U_Id) return Id;

   Null_Uid : U_Id := U_Id (Null_Unbounded_String);

   type SCM is record
      Actions : Savadur.Actions.Maps.Map;
   end record;

   function Hash (Key : Id) return Containers.Hash_Type;
   --  Renames Strings.Hash

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => SCM,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (SCM_Map : Maps.Map) return String;
   --  Return the SCM_Map image

end Savadur.SCM;
