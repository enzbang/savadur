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

with Ada.Directories;
with Savadur.Utils;

package body Copy_Tree is

   use Savadur;

   procedure Check_Copy_Tree
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Copy_Tree
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Savadur.Utils.Copy_Tree ("config", "config_duplicated");

      Assertions.Assert
        (Ada.Directories.Exists ("config_duplicated/ex_project.xml")
           and then  Ada.Directories.Exists
           ("config_duplicated/servers/another.xml"),
         "Error copy_tree failed");
   end Check_Copy_Tree;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Check copy directory");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Check_Copy_Tree'Access, "check copy tree");
   end Register_Tests;

end Copy_Tree;
