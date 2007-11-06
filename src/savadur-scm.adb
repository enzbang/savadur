with Ada.Strings.Hash;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Savadur.SCM is

   use Ada.Strings.Unbounded;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (SCM_Map : Maps.Map) return String is
      Position : Maps.Cursor := Maps.First (SCM_Map);
      Result  : Unbounded_String;
      use Ada.Text_IO;
   begin
      while Maps.Has_Element (Position) loop
         Append (Result, "* " & String (Maps.Key (Position)) & ASCII.Lf);
         Append (Result,
                 Savadur.Action.Image (Maps.Element (Position).Actions));
         Maps.Next (Position);
      end loop;

      return To_String (Result);

   end Image;

end Savadur.SCM;
