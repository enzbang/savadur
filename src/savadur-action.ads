
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;

package Savadur.Action is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;
   type U_Id is new Unbounded_String;
   type Command is new Unbounded_String;
   type Action_Type is (SCM, Default);

   type Action
     (Action_Type : Savadur.Action.Action_Type := Default) is record
      Cmd : Command;
   end record;

   type Ref_Action
      (Action_Type : Savadur.Action.Action_Type := Default) is record
      Id : U_Id;
   end record;

   function Hash (Key : Id) return Containers.Hash_Type;
   -- Renames Strings.Hash

   function Image (Action : in Savadur.Action.Action) return String;
   --  Returns action image

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => Action,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Map : in Maps.Map) return String;
   --  Returns Map image

   subtype Action_Index is Positive;

   -------------
   -- Vectors --
   -------------

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type      => Action_Index,
      Element_Type    => Ref_Action);

   function Image (Vector : Vectors.Vector) return String;
   --  Returns vector image

end Savadur.Action;
