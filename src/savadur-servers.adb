------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                      Pascal Obry - Olivier Ramonat                       --
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

package body Savadur.Servers is

   use Savadur.Utils;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Id) return Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Servers_Map : in Maps.Map) return String is
      Position : Maps.Cursor := Maps.First (Servers_Map);
      Result   : Unbounded_String;
   begin
      while Maps.Has_Element (Position) loop
         Append (Result, "* " & String (Maps.Key (Position)) & ASCII.LF);
         Append (Result, "[" & ASCII.LF);
         Append
           (Result,
            "Name => " & To_String (Maps.Element (Position).Name) & ASCII.LF);
         Append
           (Result, "URL => " &
            To_String (Maps.Element (Position).URL) & ASCII.LF);
         Append (Result, "]" & ASCII.LF);
         Maps.Next (Position);
      end loop;

      return -Result;
   end Image;

end Savadur.Servers;
