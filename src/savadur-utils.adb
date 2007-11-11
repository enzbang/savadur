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

with Ada.Text_IO;

package body Savadur.Utils is

-------------
-- Content --
-------------

   function Content (Filename : in String) return String is
      use Ada.Text_IO;

      Result : Unbounded_String;
      File   : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Filename);

      while not End_Of_File (File) loop
         Append (Result, Get_Line (File));
      end loop;

      Close (File);
      return -Result;
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

end Savadur.Utils;