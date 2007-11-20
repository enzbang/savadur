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

with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

package body Utils is

   use Ada.Strings;
   use Ada.Strings.Unbounded;

   -----------
   -- Strip --
   -----------

   function Strip (Source : in String) return String is
      Result   : Unbounded_String :=
                   To_Unbounded_String (Source);
      Position : Natural;
   begin
      loop
         Position := Index
           (Source => Result,
            Set    => Maps.To_Set (" " & ASCII.HT & ASCII.CR & ASCII.LF));
         exit when Position = 0;

         Delete (Source => Result, From => Position, Through => Position);
      end loop;

      return To_String (Result);
   end Strip;

end Utils;
