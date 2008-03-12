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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;

with Savadur.Config.Cmd;
with Savadur.Config.Filters;
with Savadur.Utils;

package Savadur.Actions is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   type Id is new Unbounded_String;
   package Id_Utils is new Generic_Utils (Source => Id);

   type Kind is (SCM, Default);

   type Result_Type is (Exit_Status, Value);

   type Action is record
      Id     : Actions.Id;
      Cmd    : Config.Cmd.Command;
      Result : Result_Type := Exit_Status;
   end record;

   Null_Action : constant Action;

   End_Action  : constant Action;
   --  Action sent when last action for a scenario

   function Image (Action : in Actions.Action) return String;
   --  Returns action image

   type On_Error_Hook is (Quit, Continue, Error);

   type Ref_Action is record
      Id             : Actions.Id;
      Action_Type    : Kind             := Default;
      Value          : Unbounded_String := Null_Unbounded_String;
      Require_Change : Boolean          := False;
      On_Error       : On_Error_Hook    := Error;
      Filters        : Config.Filters.Set;
   end record;

   Null_Ref_Action : constant Ref_Action;

   function Image (Action : in Ref_Action) return String;
   --  Returns action image

   ----------
   -- Sets --
   ----------

   function Hash (Key : in Action) return Containers.Hash_Type;
   --  Renames Strings.Hash

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Action,
      Hash                => Hash,
      Equivalent_Elements => "=");

   function Key (Element : in Action) return Id;
   --  Returns SCM id

   function Hash (Key : in Id) return Containers.Hash_Type;

   package Keys is new Sets.Generic_Keys
     (Key_Type        => Id,
      Key             => Key,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Set : in Sets.Set) return String;
   --  Returns Map image

   -------------
   -- Vectors --
   -------------

   subtype Action_Index is Positive;

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Action_Index,
      Element_Type => Ref_Action);

   function Image (Vector : in Vectors.Vector) return String;
   --  Returns vector image

private

   Null_Action     : constant Action :=
                       Action'(Id     => Id_Utils.Nil,
                               Cmd    => Config.Cmd.Null_Command,
                               Result => <>);

   End_Action      : constant Action :=
                       Action'(Id     => Id_Utils.Value ("@ENDACTION@"),
                               Cmd    => Config.Cmd.Null_Command,
                               Result => <>);

   Null_Ref_Action : constant Ref_Action :=
                       Ref_Action'(Id             => Id_Utils.Nil,
                                   Action_Type    => <>,
                                   Value          => <>,
                                   Require_Change => <>,
                                   On_Error       => <>,
                                   Filters        => Config.Filters.Null_Set);

end Savadur.Actions;
