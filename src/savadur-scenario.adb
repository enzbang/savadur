
with Ada.Strings.Hash;

package body Savadur.Scenario is

   ----------
   -- Hash --
   ----------

   function Hash (Key : Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Key));
   end Hash;

end Savadur.Scenario;
