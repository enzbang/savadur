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
with Ada.Containers.Indefinite_Vectors;

package Savadur.Actions is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;
   type U_Id is new Unbounded_String;

   function "+" (Source : Id) return U_Id;

   function "-" (Source : U_Id) return Id;

   type Command is new Unbounded_String;

   type Kind is (SCM, Default);

   type Result_Type is (Exit_Status, Value);

   type Action is record
      Cmd    : Command;
      Result : Result_Type := Exit_Status;
   end record;

   Null_Action : Action :=
                   Action'(Cmd    => Command (Null_Unbounded_String),
                           Result => <>);

   function Image (Action : in Actions.Action) return String;
   --  Returns action image

   type Ref_Action is record
      Id          : U_Id;
      Action_Type : Kind             := Default;
      Value       : Unbounded_String := Null_Unbounded_String;
   end record;

   Null_Ref_Action : Ref_Action :=
                       Ref_Action'(Id          => U_Id (Null_Unbounded_String),
                                   Action_Type => <>,
                                   Value       => <>);

   function Image (Action : in Ref_Action) return String;
   --  Returns action image

   ----------
   -- Maps --
   ----------

   function Hash (Key : Id) return Containers.Hash_Type;
   --  Renames Strings.Hash

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => Action,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Map : in Maps.Map) return String;
   --  Returns Map image

   -------------
   -- Vectors --
   -------------

   subtype Action_Index is Positive;

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type      => Action_Index,
      Element_Type    => Ref_Action);

   function Image (Vector : Vectors.Vector) return String;
   --  Returns vector image

end Savadur.Actions;
