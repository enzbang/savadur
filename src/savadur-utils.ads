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

with Ada.Strings.Unbounded;

package Savadur.Utils is

   use Ada;
   use Ada.Strings.Unbounded;

   function "+" (Source : in String) return Unbounded_String
                 renames To_Unbounded_String;

   function "-" (Source : in Unbounded_String) return String
                 renames To_String;

   generic
      type Source is new Unbounded_String;
   package Generic_Utils is

      Nil : constant Source;

      function To_Unbounded_String (S : in Source) return Unbounded_String;

      function "+" (S : in Source) return Unbounded_String
                    renames To_Unbounded_String;

      function To_String (S : in Source) return String;

      function "-" (S : in Source) return String
                    renames To_String;

      function Value (Img : in String) return Source;

   private
      Nil : constant Source := Source (Null_Unbounded_String);
   end Generic_Utils;

   Max_Characters : constant := 4_096;
   --  Maximum number of characters returned by the content routine below

   function Content
     (Filename    : in String;
      Max_Content : in Positive := Max_Characters;
      From_Top    : in Boolean := True;
      Clean       : in Boolean := False) return String;
   --  Returns a file content. Limits to 4 KB by default. If From_Top is
   --  set to False then the last Max_Characters are returned.

   procedure Set_Content (Filename, Content : in String);
   --  Sets the content of the given file

   function Parse
     (Content, Regexp : in String; N : in Positive := 1)
      return Unbounded_String;
   --  Parse content using regexp and returns the result which is the
   --  catenation of all Nth matches.

   procedure Copy_Tree (Left, Right : in String);
   --  Copies a tree

end Savadur.Utils;
