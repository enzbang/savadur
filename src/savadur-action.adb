
with Ada.Strings.Hash;

package body Savadur.Action is

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

   function Image (Action : in Savadur.Action.Action) return String is
   begin
      return To_String (Action.Cmd);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Map : in Maps.Map) return String is
      Result : Unbounded_String;

      procedure Image (Position : in Maps.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Maps.Cursor) is
      begin
         Append (Result,
                 String (Maps.Key (Position)) & " => "
                      & Image (Maps.Element (Position)));
      end Image;
   begin
      Maps.Iterate (Container => Map,
                    Process   => Image'Access);
      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Vector : in Vectors.Vector) return String is
      Result : Unbounded_String;

      procedure Image (Position : in Vectors.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Vectors.Cursor) is
      begin
         Append (Result,
                 String (Vectors.Element (Position)));
      end Image;
   begin
      Vectors.Iterate (Container => Vector,
                       Process   => Image'Access);
      return To_String (Result);
   end Image;



end Savadur.Action;
