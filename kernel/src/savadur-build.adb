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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Regpat;

with Morzhol.OS;

with Savadur.Build.Notification;
with Savadur.Client_Service.Client;
with Savadur.Config.Client;
with Savadur.Config.SCM;
with Savadur.Logs;
with Savadur.SCM;
with Savadur.Servers;
with Savadur.Utils;
with Savadur.Variables;

package body Savadur.Build is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Exceptions;

   use GNAT;
   use Savadur.Utils;

   procedure Get_Arguments
     (Command         : in     Actions.Command;
      Prog_Name       :    out Unbounded_String;
      Argument_String :    out OS_Lib.Argument_List_Access);
   --  Returns the programme name and the argument list

   function Parse
     (Project : in Projects.Project_Config;
      Cmd     : in Actions.External_Command) return Actions.External_Command;
   --  Replaces strings beginning with $
   --  by the correponding entry in project <variable> section

   function Check
     (Project     : access Projects.Project_Config;
      Exec_Action : in     Actions.Action;
      Ref         : in     Actions.Ref_Action;
      Return_Code : in     Integer;
      Log_File    : in     String) return Boolean;
   --  ???

   -----------
   -- Check --
   -----------

   function Check
     (Project     : access Projects.Project_Config;
      Exec_Action : in     Actions.Action;
      Ref         : in     Actions.Ref_Action;
      Return_Code : in     Integer;
      Log_File    : in     String) return Boolean
   is
      use type Actions.Result_Type;
      State_Directory : constant String :=
                          Projects.Project_State_Directory (Project);
      Result          : Boolean := True;
   begin
      if Exec_Action.Result = Actions.Exit_Status then
         if Ref.Value /= "" then
            Result := Return_Code = Integer'Value (-Ref.Value);
         else
            Result := Return_Code = 0;
         end if;
      end if;

      if Ref.Require_Change then
         Check_Last_State : declare
            State_Filename   : constant String :=
                                 Directories.Compose
                                   (Containing_Directory => State_Directory,
                                    Name                 =>
                                      Actions.Id_Utils.To_String (Ref.Id));
            State_File       : Text_IO.File_Type;
            Last_Exit_Status : Integer;
         begin
            --  First copy previous state if it exists, this will be needed to
            --  be able to send the blame list to notification hooks. In this
            --  case we want to be able to compute all diffs from previous run.

            if Directories.Exists (State_Filename) then
               Directories.Copy_File
                 (State_Filename, State_Filename & ".previous");
            end if;

            case Exec_Action.Result is
               when Actions.Exit_Status =>
                  --  This is a case were we check the exit status, Check with
                  --  last recored state.

                  if Directories.Exists (State_Filename) then
                     Text_IO.Open (File => State_File,
                                   Name => State_Filename,
                                   Mode => Text_IO.Out_File);

                     Integer_Text_IO.Get
                       (File => State_File, Item => Last_Exit_Status);

                     if Last_Exit_Status = Return_Code then
                        --  No change. Report error
                        Result := False;
                        Logs.Write
                          (Content => Actions.Id_Utils.To_String (Ref.Id)
                           & " has no change",
                           Kind    => Logs.Handler.Verbose);
                     end if;

                     Text_IO.Reset (File => State_File);

                  else
                     Text_IO.Create
                       (File => State_File, Name => State_Filename);
                  end if;

                  --  Write new state

                  Integer_Text_IO.Put
                    (File => State_File, Item => Return_Code);
                  Text_IO.Close (State_File);

               when Actions.Value =>
                  --  This is the case were we check for return value

                  if Directories.Exists (State_Filename)
                    and then Content (Log_File) = Content (State_Filename)
                  then
                     --  No changes. Report error

                     Result := False;
                     Logs.Write (Content => Actions.Id_Utils.To_String (Ref.Id)
                                 & " has no change " & State_Filename,
                                 Kind    => Logs.Handler.Verbose);
                  end if;

                  --  Write new state

                  Directories.Copy_File (Log_File, State_Filename);
            end case;
         end Check_Last_State;
      end if;

      if not Result then
         Logs.Write
           (Actions.External_Command_Utils.To_String
              (Exec_Action.Cmd.Cmd) & " failed");
      else
         Logs.Write (Content => "... success", Kind => Logs.Handler.Verbose);
      end if;

      return Result;
   exception
      when E : Constraint_Error =>
         raise Command_Parse_Error
           with "Value " & (-Ref.Value) & " is not an exit status";
   end Check;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Exec_Action  : in     Actions.Action;
      Directory    : in     String;
      Log_Filename : in     String;
      Return_Code  :    out Integer;
      Result       :    out Boolean)
   is
      use type Actions.Result_Type;
      use type Actions.Output_Pattern;
      use Actions.External_Command_Utils;
      use GNAT.OS_Lib;

      Exec_Path       : OS_Lib.String_Access;
      Argument_String : Argument_List_Access;
      Prog_Name       : Unbounded_String;
      Output_Filename : Unbounded_String := +Log_Filename;
   begin
      Logs.Write
        (Content => "Execute: "
         & Actions.External_Command_Utils.To_String (Exec_Action.Cmd.Cmd),
         Kind    => Logs.Handler.Verbose);
      Logs.Write
        (Content => "Log file: " & Log_Filename,
         Kind    => Logs.Handler.Very_Verbose);

      if Exec_Action.Cmd.Output /= Actions.Output_Pattern_Utils.Nil then
         Output_Filename := Output_Filename & "-tmp";
      end if;

      Get_Arguments (Exec_Action.Cmd, Prog_Name, Argument_String);

      --  Chdir to the requested base directory

      Directories.Set_Directory (Directory);

      Exec_Path := Locate_Exec_On_Path (-Prog_Name);

      if Exec_Path = null then
         raise Command_Parse_Error
           with "No Exec " & (-Prog_Name) & " on this system";
      end if;

      --  On Windows we must launch scripts using the shell

      if Morzhol.OS.Is_Windows
        and then
          (Exec_Path.all'Length < 5
           or else
             Strings.Fixed.Index (Exec_Path.all, ".exe") /=
               Exec_Path.all'Last - 3)
      then
         --  This is a Windows script, we just assume that it is a Cygwin shell
         --  script.
         --  ??? would be nice to check for .cmd and .bat that needs to be
         --  executed under cmd.exe.

         Free (Argument_String);

         Argument_String := new Argument_List (1 .. 2);
         Argument_String (1) := new String'("-c");
         Argument_String (2) :=
           new String'('"' & (-Exec_Action.Cmd.Cmd) & '"');

         Free (Exec_Path);
         Exec_Path := Locate_Exec_On_Path ("sh");

         if Exec_Path = null then
            raise Command_Parse_Error
              with "No sh shell found, can't run scripts";
         end if;

         Logs.Write
           (Content => "Execute using sh: "
              & Exec_Path.all
              & ' ' & Argument_String (1).all
              & ' ' & Argument_String (2).all,
            Kind    => Logs.Handler.Very_Verbose);

         Spawn (Program_Name => Exec_Path.all,
                Args         => Argument_String.all,
                Output_File  => To_String (Output_Filename),
                Success      => Result,
                Return_Code  => Return_Code,
                Err_To_Out   => True);

      else
         Spawn (Program_Name => Exec_Path.all,
                Args         => Argument_String.all,
                Output_File  => To_String (Output_Filename),
                Success      => Result,
                Return_Code  => Return_Code,
                Err_To_Out   => True);
      end if;

      if Exec_Action.Cmd.Output /= Actions.Output_Pattern_Utils.Nil then
         --  We need to get the content of the log and match it against the
         --  regular expression.
         Parse_Content : declare
            use type Regpat.Match_Location;

            Content : constant String :=
                        Utils.Content (To_String (Output_Filename));
            Pattern : constant Regpat.Pattern_Matcher :=
                        Regpat.Compile
                          (Actions.Output_Pattern_Utils.To_String
                             (Exec_Action.Cmd.Output));
            First   : Positive := Content'First;
            Result  : Unbounded_String;
            Matches : Regpat.Match_Array (0 .. 1);
         begin
            while First <= Content'Last loop
               Regpat.Match (Pattern, Content, Matches, Data_First => First);

               exit when Matches (0) = Regpat.No_Match
                 or else Matches (1) = Regpat.No_Match;

               --  Each result on a separate line

               if Result /= Null_Unbounded_String then
                  Append (Result, ASCII.LF);
               end if;

               Append
                 (Result, Content (Matches (1).First .. Matches (1).Last));
               First := Matches (1).Last + 1;
            end loop;

            if Result = Null_Unbounded_String then
               --  No match, just rename the output file
               if Directories.Exists (Log_Filename) then
                  Directories.Delete_File (Log_Filename);
               end if;

               Directories.Rename
                 (Old_Name => To_String (Output_Filename),
                  New_Name => Log_Filename);

            else
               Utils.Set_Content (Log_Filename, To_String (Result));

               --  And delete the previous file

               Directories.Delete_File (To_String (Output_Filename));
            end if;
         end Parse_Content;
      end if;

      if not Result then
         --  Command failed to execute
         --  Do not read the return code
         Logs.Write
           (Content => "Can not execute "
            & Actions.External_Command_Utils.To_String (Exec_Action.Cmd.Cmd),
            Kind    => Logs.Handler.Error);
      end if;

      Free (Argument_String);
      Free (Exec_Path);
   end Execute;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (Project    : in Projects.Project_Config;
      Ref_Action : in Actions.Ref_Action) return Actions.Action
   is
      use Savadur.Actions;
      Action_Id  : constant Id := Ref_Action.Id;
      Get_Action : Action := Actions.Null_Action;
   begin
      if Ref_Action.Action_Type = Actions.SCM then
         --  Search the action in the SCM list

         Search_Action : declare
            use Savadur.SCM;
            SCM_Used : constant Savadur.SCM.SCM := Savadur.SCM.Keys.Element
              (Container => Savadur.Config.SCM.Configurations,
               Key       => Project.SCM_Id);

         begin
            Get_Action := Actions.Keys.Element
              (Container => SCM_Used.Actions,
               Key       => Action_Id);
         end Search_Action;

      else
         --  Search in project action

         Get_Action := Keys.Element
           (Container => Project.Actions, Key => Action_Id);
      end if;

      --  Parse command

      Get_Action.Cmd.Cmd := Parse (Project, Get_Action.Cmd.Cmd);

      return Get_Action;
   end Get_Action;

   -------------------
   -- Get_Arguments --
   -------------------

   procedure Get_Arguments
     (Command         : in     Actions.Command;
      Prog_Name       :    out Unbounded_String;
      Argument_String :    out OS_Lib.Argument_List_Access)
   is
      use GNAT.OS_Lib;
      Command_String : constant String := -Unbounded_String (Command.Cmd);
   begin
      Extract_Program_Name : for K in Command_String'Range loop
         if Command_String (K) = ' ' then
            Prog_Name := +Command_String (Command_String'First .. K - 1);
            Argument_String := OS_Lib.Argument_String_To_List
              (Command_String (K + 1 .. Command_String'Last));
            exit Extract_Program_Name;
         end if;
      end loop Extract_Program_Name;

      if Argument_String = null then
         Prog_Name := +Command_String;
         Argument_String := OS_Lib.Argument_String_To_List ("");
      end if;
   end Get_Arguments;

   -----------
   -- Parse --
   -----------

   function Parse
     (Project : in Projects.Project_Config;
      Cmd     : in Actions.External_Command) return Actions.External_Command
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
               Append (Result, To_String (Var.Value));

               Start      := K;
               Do_Replace := False;
            end Query_Project_Variables;
         end if;
      end loop;

      if Do_Replace then
         Replace_Var : declare
            Key  : constant String := Source (Start + 1 .. Source'Last);
            Var  : constant Savadur.Variables.Variable :=
                     Savadur.Variables.Keys.Element
                       (Container => Project.Variables,
                        Key       => Savadur.Variables.
                          Name_Utils.Value (Key));
         begin
            Append (Result, To_String (Var.Value));
         end Replace_Var;

      else
         Append (Result, Source (Start .. Source'Last));
      end if;

      return Actions.External_Command (Result);
   exception
      when E : others => Logs.Write
           (Content => Exception_Information (E),
            Kind    => Logs.Handler.Error);
         raise;
   end Parse;

   ---------
   -- Run --
   ---------

   function Run
     (Project : access Projects.Project_Config;
      Server  : in     String;
      Env_Var : in     Environment_Variables.Maps.Map;
      Id      : in     Scenarios.Id;
      Job_Id  : in     Natural := 0) return Boolean
   is
      function Init return Savadur.Scenarios.Scenario;
      --  Returns the selected scenario and set the environment variables

      procedure Send_Status
        (Server_Name : in String;
         Action_Id   : in Actions.Id;
         Log_File    : in String := "");
      --  Sends the status to savadur server, this must be called only in
      --  client/server mode.

      Status : Boolean := True;

      ----------
      -- Init --
      ----------

      function Init return Savadur.Scenarios.Scenario is
         Selected_Scenario : Savadur.Scenarios.Scenario;
      begin
         Selected_Scenario := Savadur.Scenarios.Keys.Element
           (Container => Project.Scenarios,
            Key       => Id);

         --  Set environment variable for this project

         Savadur.Environment_Variables.Set_Environment (Env_Var);

         return Selected_Scenario;
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with "Scenario "
              & Savadur.Scenarios.Id_Utils.To_String (Id) & " not found";
      end Init;

      -----------------
      -- Send_Status --
      -----------------

      procedure Send_Status
        (Server_Name : in String;
         Action_Id   : in Actions.Id;
         Log_File    : in String := "")
      is

         function Log_Content return String;
         --  Returns log content or empty string

         -----------------
         -- Log_Content --
         -----------------

         function Log_Content return String is
         begin
            if Log_File = "" then
               return "";
            else
               return Content (Log_File, From_Top => False);
            end if;
         end Log_Content;

      begin
         if Config.Client_Server then

            Notify_Server : declare
               Server_URL : constant String := Servers.URL (Server_Name);
            begin
               if Server_URL = "" then
                  Logs.Write
                    ("Unable to find server endpoint for " &  Server_Name);
               end if;

               Client_Service.Client.Status
                 (Key          => Config.Client.Get_Key,
                  Project_Name =>
                    Projects.Id_Utils.To_String (Project.Project_Id),
                  Scenario     => Scenarios.Id_Utils.To_String (Id),
                  Action       => Actions.Id_Utils.To_String (Action_Id),
                  Output       => Log_Content,
                  Result       => Status,
                  Job_Id       => Job_Id,
                  Endpoint     => Server_URL);
            end Notify_Server;

         else
            Logs.Write
              (Actions.Id_Utils.To_String (Action_Id)
               & " [" & Log_Content & ']',
               Kind => Logs.Handler.Very_Verbose);
         end if;

      end Send_Status;

      Selected_Scenario : constant Savadur.Scenarios.Scenario := Init;

      Sources_Directory : constant String :=
                            Projects.Project_Sources_Directory (Project);
      Log_Directory     : constant String :=
                            Projects.Project_Log_Directory (Project);

   begin
      For_All_Ref_Actions : declare
         use Savadur.Actions.Vectors;
         Position : Cursor := Selected_Scenario.Actions.First;
      begin
         Run_Actions : while Has_Element (Position) loop

            Execute_Command : declare
               use type SCM.Id;
               Ref         : constant Actions.Ref_Action := Element (Position);
               Log_File    : constant String := Directories.Compose
                 (Containing_Directory => Log_Directory,
                  Name                 => Actions.To_String (Ref.Id));
               Exec_Action : constant Actions.Action :=
                               Get_Action (Project    => Project.all,
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
                     Status := False; --  Exit with error

                     Send_Status (Server, Ref.Id);

                     exit Run_Actions;
                  end if;

                  Status := Check (Project     => Project,
                                   Exec_Action => Exec_Action,
                                   Ref         => Ref,
                                   Return_Code => Return_Code,
                                   Log_File    => Log_File);

                  Send_Status (Server, Ref.Id, Log_File);

                  if not Status then
                     case Ref.On_Error is
                        when Actions.Quit =>
                           Status := True;
                           exit Run_Actions;

                        when Actions.Error =>
                           Project.Variables.Insert
                             (New_Item =>
                              Variables.Variable'
                                (Name =>
                                 Variables.Name_Utils.Value ("failed_action"),
                                 Value => Actions.Id_Utils.
                                   To_Unbounded_String (Ref.Id)));
                           exit Run_Actions;

                        when Actions.Continue =>
                           null;
                     end case;
                  end if;

                  Next (Position);

               else
                  --  No sources directory. This means that the project has not
                  --  been initialized.
                  --  The sources directory has to be created by the SCM
                  --  Call SCM init from current directory. If there is no SCM
                  --  defined for this project we create the directory.

                  Logs.Write ("Create directory : " & Sources_Directory);

                  if Project.SCM_Id = SCM.Null_Id then
                     Directories.Create_Directory (Sources_Directory);

                  else
                     Execute
                       (Exec_Action   => Get_Action
                          (Project    => Project.all,
                           Ref_Action => Savadur.SCM.SCM_Init),
                        Directory     =>
                          Projects.Project_Directory (Project),
                        Log_Filename  => Directories.Compose
                          (Containing_Directory => Log_Directory,
                           Name                 => "init"),
                        Return_Code   => Return_Code,
                        Result        => Result);

                     if not Result or else Return_Code /= 0
                       or else not Directories.Exists (Sources_Directory)
                     then
                        Status := False;
                        Send_Status (Server, Savadur.SCM.SCM_Init.Id);
                        raise Command_Parse_Error with "SCM init failed !";
                     end if;

                     Send_Status (Server,
                                  Savadur.SCM.SCM_Init.Id,
                                  Directories.Compose
                                    (Containing_Directory => Log_Directory,
                                     Name                 => "init"));
                  end if;
                  --  No Next (Position) to retry the same command
               end if;
            end Execute_Command;
         end loop Run_Actions;

      exception
         when Constraint_Error =>
            if Has_Element (Position) then
               raise Command_Parse_Error with "Command "
                 & Actions.To_String (Element (Position).Id) & " not found";
            else
               raise Command_Parse_Error with "Command not found";
            end if;
      end For_All_Ref_Actions;

      --  Final server notification

      Send_Status (Server, Actions.Id_Utils.Nil);

      --  Execute notifications hooks

      Build.Notification.Notify (Project, Status);

      return Status;
   end Run;

end Savadur.Build;
