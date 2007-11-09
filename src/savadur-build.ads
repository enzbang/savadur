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

with Savadur.Actions;
with Savadur.Scenarios;
with Savadur.Config.Project;
with Savadur.Environment_Variables;

package Savadur.Build is

   Command_Parse_Error : exception;

   function Execute
     (Exec_Action  : in Actions.Action;
      Check_Value  : in String;
      Directory    : in String;
      Log_Filename : in String)
      return Boolean;
   --  Executes a command. Returns False if exit status is not check_Value
   --  Before command execution, the string beginning with $ are replaced
   --  by the correponding entry in project <variable> section

   function Run
     (Project : Config.Project.Project_Config;
      Env_Var : Environment_Variables.Maps.Map;
      Id      : Scenarios.Id)
      return Boolean;
   --  Run selected scenario from project config

end Savadur.Build;
