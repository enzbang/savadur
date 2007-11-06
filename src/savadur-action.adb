
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

   function Image (Action : in Savadur.Action.Ref_Action) return String is
   begin
      return Kind'Image (Action.Action_Type) & " "
        & To_String (Action.Id);
   end Image;


   -----------
   -- Image --
   -----------

   function Image (Map : in Maps.Map) return String is
      Result : Unbounded_String := To_Unbounded_String ("[ " & ASCII.LF);

      procedure Image (Position : in Maps.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Maps.Cursor) is
      begin
         Append (Result,
                 String (Maps.Key (Position)) & " => "
                      & Image (Maps.Element (Position)) & ASCII.LF);
      end Image;
   begin
      Maps.Iterate (Container => Map,
                    Process   => Image'Access);
      Append (Result, To_Unbounded_String ("]"));
      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Vector : in Vectors.Vector) return String is
      Result : Unbounded_String := To_Unbounded_String ("[ " & ASCII.LF);

      procedure Image (Position : in Vectors.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Vectors.Cursor) is
         Element : Ref_Action := Vectors.Element (Position);
      begin
         Append (Result, Image (Element) & ASCII.LF);
      end Image;
   begin
      Vectors.Iterate (Container => Vector,
                       Process   => Image'Access);
      Append (Result, To_Unbounded_String ("]"));
      return To_String (Result);
   end Image;

end Savadur.Action;
