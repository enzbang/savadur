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

with Savadur.Action;
with Savadur.Scenario;
with Savadur.Config.Project;

package Savadur.Build is

   Command_Parse_Error : exception;

   function Execute
     (Command : Savadur.Action.Command;
      Directory : String)
      return Boolean;
   --  Executes a command. Returns False if exit status is not 0
   --  Before command execution, the string beginning with $ are replaced
   --  by the correponding entry in project <variable> section

   function Run
     (Project : Savadur.Config.Project.Project_Config;
      Id      : Savadur.Scenario.Id)
      return Boolean;
   --  Run selected scenario from project config

end Savadur.Build;
