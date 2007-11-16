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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with SHA.Process_Data;

with Savadur.Utils;

package body Savadur.Signed_Files is

   use Streams;
   use Savadur.Utils;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out Handler; Name : in String) is
      F : Stream_IO.File_Type;
   begin
      if Directories.Exists (Name) then
         Stream_IO.Open (F, Stream_IO.In_File, Name);
         File.Full_Name := +Stream_IO.Name (F);
         File.Exists := True;
         Stream_IO.Close (F);
      else
         File.Full_Name := +Name;
         raise IO_Exceptions.Name_Error
           with "File " & Name & " does not exist.";
      end if;
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists (File : in Handler) return Boolean is
   begin
      return File.Exists;
   end Exists;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (File : in Handler) return String is
   begin
      return -File.Full_Name;
   end Full_Name;

   ----------
   -- Name --
   ----------

   function Name (File : in Handler) return String is
   begin
      return Directories.Simple_Name (-File.Full_Name);
   end Name;

   ----------
   -- SHA1 --
   ----------

   function SHA1 (File : in Handler) return Signature is
      use type SHA.Strings.Hex_SHA_String;
   begin
      if not Exists (File) then
         raise IO_Exceptions.Name_Error
            with "File " & (-File.Full_Name) & " does not exist.";
      end if;

      if File.SHA1 = No_SHA1 then
         declare
            F      : Stream_IO.File_Type;
            Ctx    : SHA.Process_Data.Context;
            Digest : SHA.Digest;
            Buffer : Stream_Element_Array (1 .. 4_096);
            Last   : Stream_Element_Offset;
         begin
            Stream_IO.Open (F, Stream_IO.In_File, -File.Full_Name);

            SHA.Process_Data.Initialize (Ctx);

            while not Stream_IO.End_Of_File (F) loop
               Stream_IO.Read (F, Buffer, Last);
               exit when Last = 0;

               for K in Stream_Element_Offset range 1 .. Last loop
                  SHA.Process_Data.Add
                    (SHA.Process_Data.Byte (Buffer (K)), Ctx);
               end loop;
            end loop;

            Stream_IO.Close (F);

            SHA.Process_Data.Finalize (Digest, Ctx);
            File.Self.SHA1 := Signature
              (Characters.Handling.To_Lower
                 (String (SHA.Strings.Hex_From_SHA (Digest))));
         end;
      end if;

      return File.SHA1;
   end SHA1;

end Savadur.Signed_Files;
