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

with Config_Parse;
with Copy_Tree;
with Signed_Files;
with Periodic_Times;
with SCM_Specific;

package body Savadur_Suite is

   use AUnit.Test_Suites;

   Result : aliased Test_Suite;
   Test_1 : aliased Config_Parse.Test_Case;
   Test_2 : aliased Signed_Files.Test_Case;
   Test_3 : aliased Periodic_Times.Test_Case;
   Test_4 : aliased SCM_Specific.Test_Case;
   Test_5 : aliased Copy_Tree.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      Add_Test (Result'Access, Test_3'Access);
      Add_Test (Result'Access, Test_4'Access);
      Add_Test (Result'Access, Test_5'Access);
      return Result'Access;
   end Suite;

end Savadur_Suite;
