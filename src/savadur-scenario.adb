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
