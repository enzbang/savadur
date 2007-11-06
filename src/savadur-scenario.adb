
with Ada.Strings.Hash;

package body Savadur.Scenario is

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

   function Image (Scenario : Savadur.Scenario.Scenario) return String is
   begin
      return "Mode : "
        & To_String (Unbounded_String (Scenario.Mode)) & ASCII.Lf
        & Savadur.Action.Image (Scenario.Actions);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Scenari : in Maps.Map) return String is
      Result : Unbounded_String;

      procedure Image (Position : in Maps.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Maps.Cursor) is
      begin
         Append (Result, "* "
                 & String (Maps.Key (Position)) & ASCII.LF
                 & Image (Maps.Element (Position)));
      end Image;
   begin
      Maps.Iterate (Container => Scenari,
                    Process   => Image'Access);
      return To_String (Result);
   end Image;

end Savadur.Scenario;
