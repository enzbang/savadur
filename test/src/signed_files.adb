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

with SHA.Strings;

with Savadur.Signed_Files;

package body Signed_Files is

   procedure Check_Signed_Files
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   ------------------------
   -- Check_Signed_Files --
   ------------------------

   procedure Check_Signed_Files
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use type SHA.Strings.Hex_SHA_String;
      Filename : constant String := "config/servers/another.xml";
      H        : Savadur.Signed_Files.Handler;
   begin
      Savadur.Signed_Files.Create (H, Name => Filename);
      Assertions.Assert
        (Savadur.Signed_Files.SHA1 (H) =
           "0f3eee5c1cb57be993a14a63091c41aca754f483",
         "Wrong file signature for " & Filename
         & " found '" & String (Savadur.Signed_Files.SHA1 (H)) & ''');
   end Check_Signed_Files;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Check the signed_files API");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Check_Signed_Files'Access, "check signed files");
   end Register_Tests;

end Signed_Files;
