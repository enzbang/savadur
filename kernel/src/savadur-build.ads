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

with Savadur.Scenarios;
with Savadur.Projects;
with Savadur.Environment_Variables;

private with Savadur.Actions;

package Savadur.Build is

   Command_Parse_Error : exception;

   function Run
     (Project : access Projects.Project_Config;
      Env_Var : in     Environment_Variables.Maps.Map;
      Id      : in     Scenarios.Id) return Boolean;
   --  Runs selected scenario from project config

private

   procedure Execute
     (Exec_Action  : in     Actions.Action;
      Directory    : in     String;
      Log_Filename : in     String;
      Return_Code  :    out Integer;
      Result       :    out Boolean);
   --  Executes a command defined by Exec_Action.Cmd
   --  Before command execution, the string beginning with $ are replaced
   --  by the correponding entry in project <variable> section
   --  Success is set to True if the command is executed and its output
   --  successfully written to the file. If Success is True, then Return_Code
   --  will be set to the status code returned by the operating system.
   --  Otherwise, Return_Code is undefined.

   function Get_Action
     (Project    : in Projects.Project_Config;
      Ref_Action : in Actions.Ref_Action) return Actions.Action;
   --  Returns the action to execute matching the ref_action

end Savadur.Build;