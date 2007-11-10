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

with Ada.Containers.Indefinite_Hashed_Sets;
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
      Id          => Actions.Id (+"init"),
      Value       => <>);

   type Id is new Unbounded_String;

   package Id_Utils is new Generic_Utils (Id);

   type SCM is record
      Id      : Savadur.SCM.Id;
      Actions : Savadur.Actions.Sets.Set;
   end record;

   function Image (SCM : Savadur.SCM.SCM) return String;
   --  Returns SCM Image

   ----------
   -- Sets --
   ----------

   function Hash (Key : SCM) return Containers.Hash_Type;
   --  Renames Strings.Hash

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => SCM,
      Hash                => Hash,
      Equivalent_Elements => "=");

   function Key (Element : in SCM) return Id;
   --  Return SCM id

   function Hash (Key : Id) return Containers.Hash_Type;

   package Keys is new Sets.Generic_Keys
     (Key_Type        => Id,
      Key             => Key,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Set : Sets.Set) return String;
   --  Return the SCM set image

end Savadur.SCM;
