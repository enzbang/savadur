------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with Savadur.Config.SCM;
with Savadur.SCM;
with Savadur.Utils;

with Utils;

package body SCM_Specific is

   use Savadur;
   use Ada.Strings.Unbounded;

   procedure Check_SVN
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Git
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   ---------------
   -- Check_Git --
   ---------------

   procedure Check_Git
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      SCM     : constant Savadur.SCM.SCM := Savadur.Config.SCM.Get ("git");
      Content : constant String :=
                  Savadur.Utils.Content ("data/git-pull");
      Result  : constant String :=
                  Utils.Parse (Content, To_String (SCM.Files_Updated));
   begin
      Assertions.Assert
        (Result =
           "git-svnci" & ASCII.LF &
           "git-svnup",
         "Wrong SCM files_updated for git: regexp:'"
         & To_String (SCM.Files_Updated) & "' -> '" & Result & ''');
   end Check_Git;

   ---------------
   -- Check_SVN --
   ---------------

   procedure Check_SVN
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      SCM     : constant Savadur.SCM.SCM :=
                  Savadur.Config.SCM.Get ("subversion");
      Content : constant String :=
                  Savadur.Utils.Content ("data/svn-update");
      Result  : constant String :=
                  Utils.Parse (Content, To_String (SCM.Files_Updated));
   begin
      Assertions.Assert
        (Result =
           "sources/test/test.adb" & ASCII.LF &
         "sources/test/test.gpr" & ASCII.LF &
         "sources/morzhol.gpr" & ASCII.LF &
         "sources/.gitignore" & ASCII.LF &
         "sources/lib" & ASCII.LF &
         "sources/morzhol.check" & ASCII.LF &
         "sources/src" & ASCII.LF &
         "sources/src/morzhol-os.ads" & ASCII.LF &
         "sources/src/morzhol.ads" & ASCII.LF &
         "sources/src/morzhol-vc.adb" & ASCII.LF &
         "sources/src/morzhol-strings.adb" & ASCII.LF &
         "sources/src/morzhol-vc.ads" & ASCII.LF &
         "sources/src/morzhol-strings.ads" & ASCII.LF &
         "sources/src/morzhol-iniparser.adb" & ASCII.LF &
         "sources/src/morzhol-iniparser.ads" & ASCII.LF &
         "sources/src/morzhol-vc-rcs.adb" & ASCII.LF &
         "sources/src/morzhol-vc-rcs.ads" & ASCII.LF &
         "sources/COPYING" & ASCII.LF &
         "sources/obj" & ASCII.LF &
         "sources/makefile",
         "Wrong SCM files_updated for subversion: regexp:'"
         & To_String (SCM.Files_Updated) & "' -> '" & Result & ''');
   end Check_SVN;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Check SCM specific actions");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Check_SVN'Access, "check SCM subversion ");
      Registration.Register_Routine
        (T, Check_Git'Access, "check SCM git");
   end Register_Tests;

end SCM_Specific;
