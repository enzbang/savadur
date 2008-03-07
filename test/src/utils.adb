------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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

with GNAT.Regpat;

package body Utils is

   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use GNAT;

   -----------
   -- Parse --
   -----------

   function Parse
     (Content, Regexp : in String; N : in Positive := 1) return String
   is
      use type Regpat.Match_Location;
      Pattern : constant Regpat.Pattern_Matcher := Regpat.Compile (Regexp);
      First   : Positive := Content'First;
      Result  : Unbounded_String;
      Matches : Regpat.Match_Array (0 .. N);
   begin
      while First <= Content'Last loop
         Regpat.Match (Pattern, Content, Matches, Data_First => First);

         exit when Matches (0) = Regpat.No_Match
           or else Matches (N) = Regpat.No_Match;

         --  Each result on a separate line

         if Result /= Null_Unbounded_String then
            Append (Result, ASCII.LF);
         end if;

         Append (Result, Content (Matches (N).First .. Matches (N).Last));
         First := Matches (1).Last + 1;
      end loop;

      return To_String (Result);
   end Parse;

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
