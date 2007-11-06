with Ada.Strings.Hash;

package body Savadur.SCM is

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
