
with Ada.Strings.Hash;

package body Savadur.Action is

   ----------
   -- Hash --
   ----------

   function Hash (Key : Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Key));
   end Hash;

end Savadur.Action;
