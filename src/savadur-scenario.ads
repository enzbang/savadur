
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

with Savadur.Action;

package Savadur.Scenario is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;
   type Mode is new Unbounded_String;

   type Scenario is record
      Mode    : Savadur.Scenario.Mode;
      Actions : Savadur.Action.Vectors.Vector;
   end record;

   function Hash (Key : Id) return Containers.Hash_Type;
   -- Renames Strings.Hash

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => Scenario,
      Hash            => Hash,
      Equivalent_Keys => "=");

end Savadur.Scenario;
