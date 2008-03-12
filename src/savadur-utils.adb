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

with Ada.Characters.Handling;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

with GNAT.Regpat;

with AWS.Translator;

package body Savadur.Utils is

   use Ada.Streams;
   use AWS;
   use GNAT;

   -------------
   -- Content --
   -------------

   function Content
     (Filename    : in String;
      Max_Content : in Positive := Max_Characters;
      From_Top    : in Boolean := True;
      Clean       : in Boolean := False) return String
   is
      Buffer_Len : constant := 4_096;
      File       : Stream_IO.File_Type;
      Content    : Unbounded_String;
      Buffer     : Stream_Element_Array (1 .. Buffer_Len);
      Last       : Stream_Element_Offset;
   begin
      Stream_IO.Open
        (File => File, Mode => Stream_IO.In_File, Name => Filename);

      Read_File : while not Stream_IO.End_Of_File (File) loop
         Stream_IO.Read (File, Buffer, Last);
         exit Read_File when Last = 0;

         Append (Content, Translator.To_String (Buffer (1 .. Last)));

         --  Check max size

         if Length (Content) >= Max_Content then
            if From_Top then
               Delete (Content, Max_Content, Length (Content));
               exit Read_File;

            else
               Delete (Content, 1, Length (Content) - Max_Content);
            end if;
         end if;
      end loop Read_File;

      Stream_IO.Close (File);

      if Clean then
         while Length (Content) > 1
           and then Characters.Handling.Is_Control
             (Element (Content, Length (Content)))
         loop
            Delete (Content, Length (Content), Length (Content));
         end loop;
      end if;

      return -Content;
   end Content;

   -------------
   -- Convert --
   -------------

   package body Generic_Utils is

      ---------------
      -- To_String --
      ---------------

      function To_String (S : in Source) return String is
      begin
         return To_String (+S);
      end To_String;

      -------------------------
      -- To_Unbounded_String --
      -------------------------

      function To_Unbounded_String (S : in Source) return Unbounded_String is
      begin
         return Unbounded_String (S);
      end To_Unbounded_String;

      -----------
      -- Value --
      -----------

      function Value (Img : in String) return Source is
      begin
         return Source (Ada.Strings.Unbounded.To_Unbounded_String (Img));
      end Value;

   end Generic_Utils;

   -----------
   -- Parse --
   -----------

   function Parse
     (Content, Regexp : in String; N : in Positive := 1)
      return Unbounded_String
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

         Append
           (Result,
            Strings.Fixed.Trim
              (Content (Matches (N).First .. Matches (N).Last),
               Side => Strings.Both));
         First := Matches (1).Last + 1;
      end loop;

      return Result;
   end Parse;

   -----------------
   -- Set_Content --
   -----------------

   procedure Set_Content (Filename, Content : in String) is
      File : Stream_IO.File_Type;
   begin
      Stream_IO.Create (File, Stream_IO.Out_File, Filename);
      String'Write (Stream_IO.Stream (File), Content);
      Stream_IO.Close (File);
   end Set_Content;

end Savadur.Utils;
