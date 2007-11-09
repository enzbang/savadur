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

with Ada.Strings.Unbounded;
with Savadur.Utils;

package body Savadur.Variables is

   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   -------------
   -- Default --
   -------------

   function Default return Maps.Map is
      Default_Map : Maps.Map;
   begin
      Default_Map.Insert (Key      => "sources",
                          New_Item => "sources");

      return Default_Map;
   end Default;

   -----------
   -- Image --
   -----------

   function Image (Map : in Maps.Map) return String is
      Position : Maps.Cursor := Maps.First (Map);
      Result   : Unbounded_String := +"[" & ASCII.Lf;
   begin
      while Maps.Has_Element (Position) loop
         Append (Result, Maps.Key (Position) & " : "
                 & Maps.Element (Position) & ASCII.Lf);
         Maps.Next (Position);
      end loop;
      return -Result & "]";
   end Image;

end Savadur.Variables;
