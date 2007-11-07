------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Strings.Hash;

with Savadur.Utils;

package body Savadur.Action is

   use Savadur.Utils;

   ---------
   -- "+" --
   ---------

   function "+" (Source : Id) return U_Id is
   begin
      return U_Id (+String (Source));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Source : U_Id) return Id is
   begin
      return Id (-Unbounded_String (Source));
   end "-";

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
      Result : Unbounded_String := To_Unbounded_String ("[" & ASCII.LF);

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
      Result : Unbounded_String := To_Unbounded_String ("[" & ASCII.LF);

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
