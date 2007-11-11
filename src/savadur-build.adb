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

with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

with GNAT.OS_Lib;

with Savadur.Utils;
with Savadur.SCM;
with Savadur.Config.SCM;
with Savadur.Actions;
with Savadur.Variables;
with Savadur.Logs;

-------------------
-- Savadur.Build --
-------------------

package body Savadur.Build is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Exceptions;

   use GNAT;
   use Savadur.Utils;

   procedure Get_Arguments
     (Command         : in Actions.Command;
      Prog_Name       : out Unbounded_String;
      Argument_String : out OS_Lib.Argument_List_Access);
   --  Returns the programme name and the argument list

   function Get_Action
     (Project    : Config.Project.Project_Config;
      Ref_Action : Actions.Ref_Action)
      return Actions.Action;
   --  Returns the action to execute matching the ref_action

   function Parse
     (Project : Config.Project.Project_Config;
      Cmd     : Actions.Command)
     return Actions.Command;
   --  Replace strings beginning with $
   --  by the correponding entry in project <variable> section

   procedure Execute
     (Exec_Action  : in Actions.Action;
      Directory    : in String;
      Log_Filename : in String;
      Return_Code  : out Integer;
      Result       : out Boolean);
   --  Executes a command defined by Exec_Action.Cmd
   --  Before command execution, the string beginning with $ are replaced
   --  by the correponding entry in project <variable> section
   --  Success is set to True if the command is executed and its output
   --  successfully written to the file. If Success is True, then Return_Code
   --  will be set to the status code returned by the operating system.
   --  Otherwise, Return_Code is undefined.

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Exec_Action  : in Actions.Action;
      Directory    : in String;
      Log_Filename : in String;
      Return_Code  : out Integer;
      Result       : out Boolean)
   is
      use GNAT.OS_Lib;
      use type Actions.Result_Type;
      Exec_Path       : OS_Lib.String_Access;
      Argument_String : Argument_List_Access;
      Prog_Name       : Unbounded_String;
   begin

      Logs.Write ("Execute "
                  & Actions.Command_Utils.To_String (Exec_Action.Cmd),
                  Logs.Verbose);

      Get_Arguments (Exec_Action.Cmd, Prog_Name, Argument_String);

      --  Chdir to the requested base directory

      Directories.Set_Directory (Directory);

      Exec_Path := Locate_Exec_On_Path (-Prog_Name);

      if Exec_Path = null then
         raise Command_Parse_Error with "No Exec " & (-Prog_Name)
           & " on this system";
      end if;

      Spawn (Program_Name => Exec_Path.all,
             Args         => Argument_String.all,
             Output_File  => Log_Filename,
             Success      => Result,
             Return_Code  => Return_Code,
             Err_To_Out   => True);

      if not Result then
         --  Command failed to execute
         --  Do not read the return code
         Logs.Write ("Can not execute "
                     & Actions.Command_Utils.To_String (Exec_Action.Cmd),
                     Logs.Error);
      else
         Free (Argument_String);
         Free (Exec_Path);
      end if;
   exception
      when E : others => Logs.Write
           (Content => Exception_Information (E),
            Kind    => Logs.Error);
         raise;
   end Execute;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (Project    : Config.Project.Project_Config;
      Ref_Action : Actions.Ref_Action)
      return Actions.Action
   is
      use Savadur.Actions;
      Action_Id  : constant Id := Ref_Action.Id;
      Get_Action : Action;
   begin

      if Ref_Action.Action_Type = Actions.SCM then
         --  Search the action in the SCM list

         declare
            use Savadur.SCM;
            SCM_Used : Savadur.SCM.SCM := Savadur.SCM.Keys.Element
              (Container => Savadur.Config.SCM.Configurations,
               Key       => Project.SCM_Id);
         begin

            Get_Action := Actions.Keys.Element
              (Container => SCM_Used.Actions,
               Key       => Action_Id);

         end;
      else
         --  Search in project action

         Get_Action := Keys.Element (Container => Project.Actions,
                                     Key       => Action_Id);
      end if;

      --  Parse command
      Get_Action.Cmd := Parse (Project, Get_Action.Cmd);

      return Get_Action;
   end Get_Action;

   -------------------
   -- Get_Arguments --
   -------------------

   procedure Get_Arguments
     (Command         : in Actions.Command;
      Prog_Name       : out Unbounded_String;
      Argument_String : out OS_Lib.Argument_List_Access)
   is
      use GNAT.OS_Lib;
      Command_String : constant String := -Unbounded_String (Command);
   begin
      for K in Command_String'Range loop
         if Command_String (K) = ' ' then
            Prog_Name := +Command_String (Command_String'First .. K - 1);
            Argument_String := OS_Lib.Argument_String_To_List
              (Command_String (K + 1 .. Command_String'Last));
            exit;
         end if;
      end loop;

      if Argument_String = null then
         Prog_Name := +Command_String;
         Argument_String := OS_Lib.Argument_String_To_List ("");
      end if;
   end Get_Arguments;

   -----------
   -- Parse --
   -----------

   function Parse
     (Project : Config.Project.Project_Config;
      Cmd     : Actions.Command)
      return Actions.Command
   is
      Source     : constant String := -Unbounded_String (Cmd);
      Start      : Positive := Source'First;
      Do_Replace : Boolean := False;
      Result     : Unbounded_String;
   begin
      for K in Source'Range loop
         if Source (K) = '$' then
            Append (Result, Source (Start .. K - 1));
            Start      := K;
            Do_Replace := True;
         elsif Do_Replace and then Source (K) = ' ' then
            Query_Project_Variables : declare
               Key  : constant String := Source (Start + 1 .. K - 1);
               Var  : constant Savadur.Variables.Variable :=
                        Savadur.Variables.Keys.Element
                          (Container => Project.Variables,
                           Key       => Savadur.Variables.
                             Name_Utils.Value (Key));
            begin
               Append (Result,
                       To_String (Var.Value));

               Start      := K;
               Do_Replace := False;
            end Query_Project_Variables;
         end if;
      end loop;

      if Do_Replace then
         declare
            Key  : constant String := Source (Start + 1 .. Source'Last);
            Var  : constant Savadur.Variables.Variable :=
                     Savadur.Variables.Keys.Element
                       (Container => Project.Variables,
                        Key       => Savadur.Variables.
                          Name_Utils.Value (Key));
         begin
            Append (Result,
                    To_String (Var.Value));
         end;

      else
         Append (Result, Source (Start .. Source'Last));
      end if;

      return Actions.Command (Result);
   exception
      when E : others => Logs.Write
           (Content => Exception_Information (E),
            Kind    => Logs.Error);
         raise;
   end Parse;

   ---------
   -- Run --
   ---------

   function Run
     (Project : Config.Project.Project_Config;
      Env_Var : Environment_Variables.Maps.Map;
      Id      : Scenarios.Id)
      return Boolean
   is
      Selected_Scenario : Scenarios.Scenario;
      Sources_Directory : constant String :=
                            Config.Project.Project_Sources_Directory (Project);
      State_Directory   : constant String :=
                            Config.Project.Project_Log_Directory
                              (Project.Project_Id);
      Log_Directory     : constant String :=
                            Config.Project.Project_Log_Directory
                              (Project.Project_Id);
      Success           : Boolean := True;
   begin
      Get_Selected_Scenario : begin
         Selected_Scenario := Savadur.Scenarios.Keys.Element
           (Container => Project.Scenarios,
            Key       => Id);
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with " Scenario "
              & Savadur.Scenarios.Id_Utils.To_String (Id) & " not found";
      end Get_Selected_Scenario;

      --  Set environment variable for this project

      Savadur.Environment_Variables.Set_Environment (Env_Var);

      For_All_Ref_Actions : declare
         use Savadur.Actions;
         use Savadur.Actions.Vectors;
         Position : Cursor := Selected_Scenario.Actions.First;
      begin
         while Has_Element (Position) loop

            Execute_Command : declare
               Ref         : constant Ref_Action := Element (Position);
               Log_File    : constant String     := Directories.Compose
                 (Containing_Directory => Log_Directory,
                  Name                 => To_String (Ref.Id));
               Exec_Action : constant Action    :=
                               Get_Action (Project    => Project,
                                           Ref_Action => Ref);
               Return_Code : Integer;
               Result      : Boolean;
            begin
               if Directories.Exists (Sources_Directory) then

                  Execute (Exec_Action  => Exec_Action,
                           Directory    => Sources_Directory,
                           Log_Filename => Log_File,
                           Return_Code  => Return_Code,
                           Result       => Result);

                  if not Result then
                     Success := False;
                  end if;

                  if Ref.Require_Change then
                     if Exec_Action.Result = Actions.Exit_Status then
                        declare
                           State_Filename : constant String
                             := Directories.Compose
                               (Containing_Directory => State_Directory,
                                Name                 => To_String (Ref.Id));
                           State_File : Text_IO.File_Type;
                        begin
                           Text_IO.Create (File => State_File,
                                           Name => State_Filename);

                           Integer_Text_IO.Put (File => State_File,
                                                Item => Return_Code);
                           Text_IO.Close (State_File);
                        end;
                     else
                        --  ??? Filter this result

                        declare
                           State_Filename : constant String
                             := Directories.Compose
                               (Containing_Directory => State_Directory,
                                Name                 => To_String (Ref.Id));
                        begin
                           Directories.Copy_File (Log_File, State_Filename);
                        end;
                     end if;
                  end if;
                  if not Result then
                     --  Stop on failure by default
                     exit;
                  end if;
                  Next (Position);
               else
                  --  No sources directory. This means that the project has not
                  --  been initialized.
                  --  The sources directory has to be created by the SCM
                  --  Call SCM init from current directory

                  Logs.Write
                    (Content => "Create directory : " & Sources_Directory,
                     Kind    => Logs.Error);

                  Execute
                    (Exec_Action   => Get_Action
                       (Project    => Project,
                        Ref_Action => Savadur.SCM.SCM_Init),
                     Directory     => Config.Project.Project_Directory
                       (Project.Project_Id),
                     Log_Filename  => Directories.Compose
                       (Containing_Directory => Log_Directory,
                        Name                 => "init"),
                     Return_Code   => Return_Code,
                     Result        => Result);

                  if not Result or else Return_Code /= 0
                    or else Directories.Exists (Sources_Directory)
                  then
                     raise Command_Parse_Error with " SCM init failed !";
                  end if;

                  --  No Next (Position) to retry the same command
               end if;
            end Execute_Command;
         end loop;
      exception
         when Constraint_Error =>
            if Has_Element (Position) then
               raise Command_Parse_Error with " Command "
                 & To_String (Element (Position).Id) & " not found";
            else
               raise Command_Parse_Error with " Command not found";
            end if;
      end For_All_Ref_Actions;
      return Success;
   end Run;

end Savadur.Build;
