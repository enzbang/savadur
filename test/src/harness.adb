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

with Ada.Command_Line;
with Ada.Directories;

with AUnit;
with AUnit_Framework;
with Savadur_Suite;

with Savadur.Config;
with Ada.Text_IO;

-------------
-- Harness --
-------------

procedure Harness is

   use Ada;
   use AUnit_Framework;

   function Run is new
     AUnit.Test_Runner_With_Status (Savadur_Suite.Suite);

begin

   Savadur.Config.Set_Savadur_Directory
     (Directories.Compose
        (Containing_Directory => Directories.Current_Directory,
         Name                 => "test-dir"));

   Ada.Text_IO.Put_Line (Directories.Current_Directory);

   if Run = Failure then
         Command_Line.Set_Exit_Status (Command_Line.Failure);
   end if;
end Harness;
