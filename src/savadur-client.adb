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
with Ada.Strings.Unbounded;

with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

procedure Savadur.Client is
   Project : Savadur.Config.Project.Project_Config :=
               Savadur.Config.Project.Parse ("test/savadurrc");

   SCM_Map : Savadur.SCM.Maps.Map := Savadur.Config.SCM.Parse ("config/scm");

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
begin

   Put_Line ("Savadur client");
   New_Line;
   Put_Line ("SCM : " & To_String (Unbounded_String (Project.SCM)));
   New_Line;
   Put_Line ("Action list : ");
   New_Line;
   Put_Line (Savadur.Action.Image (Project.Actions));
   New_Line;
   Put_Line ("Scenari : ");
   New_Line;
   Put_Line (Savadur.Scenario.Image (Project.Scenari));
   New_Line;
   New_Line;
   New_Line;
   Put_Line ("SCM Found");
   New_Line;
   Put_Line (Savadur.SCM.Image (SCM_Map));

end Savadur.Client;
