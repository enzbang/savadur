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
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

with SHA.Process_Data;

with Savadur.Utils;

package body Savadur.Signed_Files is

   use Streams;
   use Savadur.Utils;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out Handler; Name, Filename : in String) is
      F      : Stream_IO.File_Type;
      Ctx    : SHA.Process_Data.Context;
      Digest : SHA.Digest;
      Buffer : Stream_Element_Array (1 .. 4_096);
      Last   : Stream_Element_Offset;
   begin
      File.Name := +Name;

      if Filename /= "" and then Directories.Exists (Filename) then
         Stream_IO.Open (F, Stream_IO.In_File, Filename);
         File.Full_Name := +Stream_IO.Name (F);

         --  Compute SHA1

         SHA.Process_Data.Initialize (Ctx);

         Process_File : while not Stream_IO.End_Of_File (F) loop
            Stream_IO.Read (F, Buffer, Last);
            exit Process_File when Last = 0;

            Process_Data : for K in Stream_Element_Offset range 1 .. Last loop
               SHA.Process_Data.Add
                 (SHA.Process_Data.Byte (Buffer (K)), Ctx);
            end loop Process_Data;
         end loop Process_File;

         Stream_IO.Close (F);

         SHA.Process_Data.Finalize (Digest, Ctx);
         File.SHA1 := Signature
           (Characters.Handling.To_Lower
              (String (SHA.Strings.Hex_From_SHA (Digest))));

         File.Exists := True;

      else
         File.SHA1 := No_SHA1;
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
      return -File.Name;
   end Name;

   ----------
   -- SHA1 --
   ----------

   function SHA1 (File : in Handler) return Signature is
   begin
      return File.SHA1;
   end SHA1;

   -------------------------
   -- To_External_Handler --
   -------------------------

   function To_External_Handler
     (File : in Handler) return External_Handler is
   begin
      return External_Handler (Name (File))
        & "@" & External_Handler (SHA1 (File));
   end To_External_Handler;

   ----------------
   -- To_Handler --
   ----------------

   function To_Handler (File : in External_Handler) return Handler is
      K : constant Natural := Strings.Fixed.Index (String (File), "@");
   begin
      return H : Handler do
         H.Name  := +String (File (File'First .. K - 1));
         H.SHA1  := Signature (File (K + 1 .. File'Last));
      end return;
   end To_Handler;

end Savadur.Signed_Files;
