
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;

with Savadur.Action;

package Savadur.SCM is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;
   type U_Id is new Unbounded_String;

   Null_Uid : U_Id := U_Id (Null_Unbounded_String);

   type SCM is record
      Actions : Savadur.Action.Vectors.Vector;
   end record;

   function Hash (Key : Id) return Containers.Hash_Type;
   -- Renames Strings.Hash

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => SCM,
      Hash            => Hash,
      Equivalent_Keys => "=");

end Savadur.SCM;
