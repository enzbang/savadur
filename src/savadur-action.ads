
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;

package Savadur.Action is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;
   type Command is new Unbounded_String;
   type Action_Type is (SCM, Default);

   type Action
     (Action_Type : Savadur.Action.Action_Type := Default) is record
      Cmd : Command;
   end record;

   function Hash (Key : Id) return Containers.Hash_Type;
   -- Renames Strings.Hash

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => Action,
      Hash            => Hash,
      Equivalent_Keys => "=");

   subtype Action_Index is Positive;

   -------------
   -- Vectors --
   -------------

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type      => Action_Index,
      Element_Type    => Id);

end Savadur.Action;
