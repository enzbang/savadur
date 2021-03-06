------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.OS_Lib;

with AWS.Utils;
with Morzhol.OS;

with Savadur.Build.Notification;
with Savadur.Client_Service.Client;
with Savadur.Config.Client;
with Savadur.Config.Cmd;
with Savadur.Config.Committers;
with Savadur.Config.Filters;
with Savadur.Config.SCM;
with Savadur.Logs;
with Savadur.SCM;
with Savadur.Servers;
with Savadur.Utils;
with Savadur.Web_Services.Client;

package body Savadur.Build is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Exceptions;

   use GNAT;
   use Savadur.Utils;

   procedure Get_Arguments
     (Command         : in     Config.Cmd.Command;
      Prog_Name       :    out Unbounded_String;
      Argument_String :    out OS_Lib.Argument_List_Access);
   --  Returns the programme name and the argument list

   function Check
     (Project     : not null access Projects.Project_Config;
      Exec_Action : in Actions.Action;
      Ref         : in Actions.Ref_Action;
      Return_Code : in Integer;
      Job_Id      : in Natural;
      Log_File    : in String;
      Diff_Data   : not null access Web_Services.Client.Diff_Data)
      return Scenarios.Run_Status;
   --  ???

   procedure Apply_Filters
     (Filename : in String;
      Filters  : in Config.Filters.Set);
   --  Apply all filters from the content of the given filename. Write output
   --  into file names computed from the filename and the filter name as
   --  follow: <filename>.<filter_name>.

   -------------------
   -- Apply_Filters --
   -------------------

   procedure Apply_Filters
     (Filename : in String;
      Filters  : in Config.Filters.Set)
   is
      use type Config.Filters.Filter_Id;
   begin
      for K in Filters'Range loop
         if not (Filters (K) = Config.Filters.Id_Utils.Nil) then
            Logs.Write (Content => "Apply_Filters on " & Filename,
                        Kind    => Logs.Handler.Very_Verbose);

            Apply_Filter : declare
               Content       : constant String := Utils.Content (Filename);
               Filter        : constant Config.Filters.Filter :=
                                 Config.Filters.Get (Filters (K));
               Filter_Output : constant String :=
                                 Filename & "."
                                   & Config.Filters.Simple_Name (Filter.Id);
               Result        : Unbounded_String;
            begin
               Result := Utils.Parse
                 (Content,
                  Config.Filters.Pattern_Utils.To_String (Filter.Pattern));

               Utils.Set_Content (Filter_Output, To_String (Result));
            end Apply_Filter;
         end if;
      end loop;

   exception
      when Text_IO.Name_Error =>
         Logs.Write (Content => "Cannot open file : " & Filename,
                     Kind    => Logs.Handler.Error);
   end Apply_Filters;

   -----------
   -- Check --
   -----------

   function Check
     (Project     : not null access Projects.Project_Config;
      Exec_Action : in Actions.Action;
      Ref         : in Actions.Ref_Action;
      Return_Code : in Integer;
      Job_Id      : in Natural;
      Log_File    : in String;
      Diff_Data   : not null access Web_Services.Client.Diff_Data)
      return Scenarios.Run_Status
   is
      use type Actions.Result_Type;
      use type Scenarios.Run_Status;

      function Filter_Based (Status : in String) return Boolean;
      --  Returns Success if the status if based on filter's result

      ------------------
      -- Filter_Based --
      ------------------

      function Filter_Based (Status : in String) return Boolean is
      begin
         return Strings.Fixed.Index (Status, "=") /= 0;
      end Filter_Based;

      State_Directory : constant String :=
                          Projects.Project_State_Directory (Project);
      Result          : Scenarios.Run_Status := Scenarios.Success;
   begin
      --  Set the exit status from the return code

      if Exec_Action.Result = Actions.Exit_Status then
         if Ref.Value /= "" then
            Get_Return_Code : begin
               if Return_Code = Integer'Value (-Ref.Value) then
                  Result := Scenarios.Success;
               else
                  Result := Scenarios.Failure;
               end if;
            exception
               when Constraint_Error =>
                  raise Command_Parse_Error
                    with "Value " & (-Ref.Value) & " is not an exit status";
            end Get_Return_Code;

         else
            if Return_Code = 0 then
               Result := Scenarios.Success;
            else
               Result := Scenarios.Failure;
            end if;
         end if;
      end if;

      --  Apply filters if any

      Apply_Filters
        (Log_Filename (Project, Exec_Action.Id, Job_Id), Ref.Filters);

      --  Check filter result, should we continue or exit now

      if Filter_Based (-Ref.Status) then
         Check_Filters : declare
            function Get_Filter
              (Name : in String) return Config.Filters.Filter;
            --  Returns the filter give the name. First look in the project
            --  then in the inherited SCM.

            ----------------
            -- Get_Filter --
            ----------------

            function Get_Filter
              (Name : in String) return Config.Filters.Filter
            is
               use type Config.Filters.Filter;
               Filter : Config.Filters.Filter;
            begin
               Filter := Config.Filters.Get
                 (Config.Filters.Get_Id
                    (Projects.Id_Utils.To_String (Project.Project_Id), Name));

               if Filter = Config.Filters.Null_Filter then
                  Filter := Config.Filters.Get
                    (Config.Filters.Get_Id
                       (SCM.Id_Utils.To_String (Project.SCM_Id), Name));
               end if;

               if Filter = Config.Filters.Null_Filter then
                  Logs.Write (Content => "Unknown filter " & Name,
                              Kind    => Logs.Handler.Error);
                  raise Command_Parse_Error with "Unknown filter " & Name;
               end if;

               return Filter;
            end Get_Filter;

            Status           : constant String := -Ref.Status;
            Log_File         : constant String :=
                                 Log_Filename
                                   (Project, Exec_Action.Id, Job_Id);
            Sep              : constant Positive :=
                                 Strings.Fixed.Index (Status, "=");
            F1_Name, F2_Name : Unbounded_String;
            Filter1, Filter2 : Config.Filters.Filter;
            Equal            : Boolean;
         begin
            --  Get filter names

            if Status (Sep - 1) = '/' then
               F1_Name := +Status (Status'First .. Sep - 2);
               Equal := False;
            else
               F1_Name := +Status (Status'First .. Sep - 1);
               Equal := True;
            end if;

            F2_Name := +Status (Sep + 1 .. Status'Last);

            --  Get filter objects

            Filter1 := Get_Filter (-F1_Name);
            Filter2 := Get_Filter (-F2_Name);

            Compare_Filters_Results : declare
               L1 : constant String :=
                      Utils.Content
                        (Log_File & "."
                         & Config.Filters.Simple_Name (Filter1.Id));
               L2 : constant String :=
                      Utils.Content
                        (Log_File & "."
                         & Config.Filters.Simple_Name (Filter2.Id));
            begin
               if (L1 = L2) = Equal then
                  --  Ok if both logs are equal and check is equality
                  Result := Scenarios.Success;

               else
                  Result := Scenarios.Failure;
                  Logs.Write
                    (Content => Actions.Id_Utils.To_String (Ref.Id)
                     & " filters check is false: " & Status,
                     Kind    => Logs.Handler.Verbose);
               end if;
            end Compare_Filters_Results;
         end Check_Filters;

      elsif Ref.Status = "require_change" then
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
               Diff_Data.V1 := +Content (State_Filename, Clean => True);
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
                        Result := Scenarios.Failure;
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

                     Result := Scenarios.Failure;
                     Logs.Write (Content => Actions.Id_Utils.To_String (Ref.Id)
                                 & " has no change " & State_Filename,
                                 Kind    => Logs.Handler.Verbose);
                  end if;

                  --  Write new state

                  Directories.Copy_File (Log_File, State_Filename);
            end case;

            Diff_Data.V2 := +Content (State_Filename, Clean => True);
         end Check_Last_State;

         --  Compute the blame (committers) list
         --  Diff_Data V1 and V2 contains the version before and after the
         --  update. Call the special SCM action committers_list to get all
         --  committers for those specific modifications.

         Set_Versions_Var : declare
            Vars   : Variables.Sets.Set;
            Action : Actions.Action;
         begin
            if Diff_Data.V1 = Null_Unbounded_String then
               --  First time we initialize/run this project
               Vars.Insert
                 (Variables.Variable'
                    (Variables.Name_Utils.Value ("v1"),
                     Diff_Data.V2));
               Action := Get_Action (Project    => Project.all,
                                     Ref_Action => SCM.Committers_1,
                                     Vars       => Vars);

            else
               Vars.Insert
                 (Variables.Variable'
                    (Variables.Name_Utils.Value ("v1"),
                     Diff_Data.V1));
               Vars.Insert
                 (Variables.Variable'
                    (Variables.Name_Utils.Value ("v2"),
                     Diff_Data.V2));
               Action := Get_Action (Project    => Project.all,
                                     Ref_Action => SCM.Committers_N,
                                     Vars       => Vars);
            end if;

            Get_Committers : declare
               Sources_Directory : constant String :=
                                     Projects.Project_Sources_Directory
                                       (Project);
               Log_File          : constant String :=
                                     Log_Filename (Project, Action.Id, Job_Id);
               Return_Code       : Integer;
               Result            : Boolean;
               File              : Text_IO.File_Type;
               Buffer            : String (1 .. 1_024);
               Last              : Natural;
               Committers        : Web_Services.Client.Name_Set (1 .. 100);
               --  Max 100 committers, ??? should not be hard coded
               K                 : Natural := Committers'First - 1;
            begin
               Execute (Exec_Action  => Action,
                        Directory    => Sources_Directory,
                        Log_Filename => Log_File,
                        Return_Code  => Return_Code,
                        Result       => Result);

               --  Now load the file and create the committers list (we have
               --  one committer on each line).

               Text_IO.Open (File, Text_IO.In_File, Log_File);

               while not Text_IO.End_Of_File (File) loop
                  Text_IO.Get_Line (File, Buffer, Last);
                  if Last > 0 then
                     K := K + 1;
                     Committers (K) := To_Unbounded_String
                       (Config.Committers.Get (Buffer (1 .. Last)));
                  end if;
               end loop;

               Text_IO.Close (File);

               --  Set committers array

               Diff_Data.Committers :=
                 Web_Services.Client.Name_Set_Safe_Pointer.To_Safe_Pointer
                   (Committers (1 .. K));
            end Get_Committers;
         end Set_Versions_Var;
      end if;

      if Result = Scenarios.Failure then
         Logs.Write
           (Config.Cmd.External_Command_Utils.To_String
              (Exec_Action.Cmd.Cmd) & " failed");
      else
         Logs.Write (Content => "... success", Kind => Logs.Handler.Verbose);
      end if;

      return Result;
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
      use type Config.Cmd.Output_Pattern;
      use Config.Cmd.External_Command_Utils;
      use GNAT.OS_Lib;

      Exec_Path       : OS_Lib.String_Access;
      Argument_String : Argument_List_Access;
      Prog_Name       : Unbounded_String;
      Output_Filename : Unbounded_String := +Log_Filename;
   begin
      Logs.Write
        (Content => "Execute: "
         & Config.Cmd.External_Command_Utils.To_String (Exec_Action.Cmd.Cmd),
         Kind    => Logs.Handler.Verbose);
      Logs.Write
        (Content => "Log file: " & Log_Filename,
         Kind    => Logs.Handler.Very_Verbose);

      if Exec_Action.Cmd.Output /= Config.Cmd.Output_Pattern_Utils.Nil then
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

      --  First apply the main output filter

      if Exec_Action.Cmd.Output /= Config.Cmd.Output_Pattern_Utils.Nil then
         --  We need to get the content of the log and match it against the
         --  regular expression.
         Parse_Content : declare
            Result : Unbounded_String;
         begin
            Result := Utils.Parse
              (Utils.Content (To_String (Output_Filename)),
               Config.Cmd.Output_Pattern_Utils.To_String
                 (Exec_Action.Cmd.Output));

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

      --  Now apply all filters

      Apply_Filters (To_String (Output_Filename), Exec_Action.Cmd.Filters);

      if not Result then
         --  Command failed to execute, do not read the return code
         Logs.Write
           (Content => "Can not execute " &
             Config.Cmd.External_Command_Utils.To_String (Exec_Action.Cmd.Cmd),
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
      Ref_Action : in Actions.Ref_Action;
      Vars       : in Variables.Sets.Set := Variables.Sets.Empty_Set)
      return Actions.Action
   is
      use Savadur.Actions;

      function Parse
        (Project : in Projects.Project_Config;
         Cmd     : in Config.Cmd.External_Command)
         return Config.Cmd.External_Command;
      --  Replaces strings beginning with $
      --  by the correponding entry in project <variable> section.

      -----------
      -- Parse --
      -----------

      function Parse
        (Project : in Projects.Project_Config;
         Cmd     : in Config.Cmd.External_Command)
         return Config.Cmd.External_Command
      is

         function Get_Value (Key : in String) return Variables.Variable;
         --  Returns Key's value by looking into local Vars or project
         --  variables.

         ---------------
         -- Get_Value --
         ---------------

         function Get_Value (Key : in String) return Variables.Variable is
            Name : constant Variables.Name := Variables.Name_Utils.Value (Key);
         begin
            if Variables.Keys.Contains (Vars, Name) then
               return Variables.Keys.Element
                 (Container => Vars, Key => Name);

            elsif Variables.Keys.Contains (Project.Variables, Name) then
               return Variables.Keys.Element
                 (Container => Project.Variables, Key => Name);

            else
               return Variables.Variable'
                 (Name => Name, Value => Null_Unbounded_String);
            end if;
         end Get_Value;

         Source     : constant String := -Unbounded_String (Cmd);
         Start      : Positive := Source'First;
         Do_Replace : Boolean  := False;
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
                           Get_Value (Key);
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
                        Get_Value (Key);
            begin
               Append (Result, To_String (Var.Value));
            end Replace_Var;

         else
            Append (Result, Source (Start .. Source'Last));
         end if;

         return Config.Cmd.External_Command (Result);
      exception
         when E : others =>
            Logs.Write
              (Content => Exception_Information (E),
               Kind    => Logs.Handler.Error);
            raise;
      end Parse;

      Action_Id  : constant Id := Ref_Action.Id;
      Get_Action : Action := Actions.Null_Action;
   begin
      if Ref_Action.Action_Type = Actions.SCM then
         --  Search the action in the SCM list

         Search_Action : declare
            use Savadur.SCM;
            Project_SCM : constant Savadur.SCM.SCM := Savadur.SCM.Keys.Element
              (Container => Savadur.Config.SCM.Configurations,
               Key       => Project.SCM_Id);

         begin
            Get_Action := Actions.Keys.Element
              (Container => Project_SCM.Actions,
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
     (Command         : in     Config.Cmd.Command;
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

   ------------------
   -- Log_Filename --
   ------------------

   function Log_Filename
     (Project   : access Projects.Project_Config;
      Action_Id : in     Actions.Id;
      Job_Id    : in     Natural;
      Directory : in     String := "";
      Prefix    : in     String := "") return String
   is
      Log_Directory : constant String :=
                        Projects.Project_Log_Directory (Project);
      A_Id          : constant String :=
                        Characters.Handling.To_Lower
                          (Actions.To_String (Action_Id));
   begin
      if Directory = "" then
         return Directories.Compose
           (Containing_Directory => Log_Directory,
            Name                 =>
              AWS.Utils.Image (Job_Id) & "-" & Prefix & A_Id);

      else
         return Directories.Compose
           (Containing_Directory => Directory,
            Name                 =>
              AWS.Utils.Image (Job_Id) & "-" & Prefix & A_Id);
      end if;
   end Log_Filename;

   ---------
   -- Run --
   ---------

   function Run
     (Project : access Projects.Project_Config;
      Patch   : in     String;
      Server  : in     String;
      Env_Var : in     Environment_Variables.Containers.Maps.Map;
      Id      : in     Scenarios.Id;
      Job_Id  : in     Natural := 0) return Scenarios.Run_Status
   is
      use type SCM.Id;

      function Init return Savadur.Scenarios.Scenario;
      --  Returns the selected scenario and set the environment variables

      procedure Send_Status
        (Server_Name : in String;
         Action_Id   : in Actions.Id;
         Log_File    : in String := "");
      --  Sends the status to savadur server, this must be called only in
      --  client/server mode.

      Status    : Scenarios.Run_Status := Scenarios.Success;
      Diff_Data : aliased Web_Services.Client.Diff_Data;
      Run_Vars  : Variables.Sets.Set;
      Last_Log  : Unbounded_String;

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

         if Patch /= "" then
            Ada.Environment_Variables.Set
              (Name  => "PATCH",
               Value => Directories.Compose
                 (Containing_Directory => Savadur.Config.Patch_Directory,
                  Name                 => Patch));
         end if;

         Savadur.Environment_Variables.Containers.Set_Environment (Env_Var);

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

         function Log_Content (Activated : in Boolean := True) return String;
         --  Returns log content or empty string

         -----------------
         -- Log_Content --
         -----------------

         function Log_Content (Activated : in Boolean := True) return String is
         begin
            if Log_File = "" or else not Activated then
               return "";
            else
               return Content (Log_File, From_Top => False);
            end if;
         end Log_Content;

         L_Filename : Unbounded_String;

      begin
         if Config.Client_Server then

            Notify_Server : declare
               Server     : constant Servers.Server :=
                              Servers.Get (Server_Name);
               Server_URL : constant String := Servers.URL (Server);
            begin
               if Server_URL = "" then
                  Logs.Write
                    ("Unable to find server endpoint for " &  Server_Name);
               end if;

               if Servers.Log_Path (Server) /= "" then
                  --  Write log to this location
                  if Directories.Exists (Servers.Log_Path (Server)) then
                     L_Filename := +Log_Filename
                         (Project, Action_Id, Job_Id,
                          Directory => Servers.Log_Path (Server),
                          Prefix    => Servers.Log_Prefix (Server));

                     Utils.Set_Content (-L_Filename, Log_Content);
                  else
                     Logs.Write (Content => "log directory does not exists: "
                                   & Servers.Log_Path (Server),
                                 Kind    => Logs.Handler.Warnings);
                  end if;

               else
                  L_Filename := +Log_File;
               end if;

               Client_Service.Client.Status
                 (Key          => Config.Client.Get_Key,
                  Project_Name =>
                    Projects.Id_Utils.To_String (Project.Project_Id),
                  Scenario     => Scenarios.Id_Utils.To_String (Id),
                  Action       => Actions.Id_Utils.To_String (Action_Id),
                  Log_Filename => -L_Filename,
                  Output       => Log_Content
                    (Activated => Servers.Send_Log (Server)),
                  Result       => Scenarios.Run_Status'Pos (Status),
                  Job_Id       => Job_Id,
                  Diff_Data    => Web_Services.Client.No_Diff_Data,
                  Endpoint     => Server_URL);
            end Notify_Server;

         else
            Logs.Write (Content => Actions.Id_Utils.To_String (Action_Id)
                          & " [" & Log_Content & ']',
                        Kind => Logs.Handler.Very_Verbose);
         end if;
      end Send_Status;

      Selected_Scenario : constant Savadur.Scenarios.Scenario := Init;

      Sources_Directory : Unbounded_String :=
                            +Projects.Project_Sources_Directory (Project);

   begin

      if Patch /= "" then
         --  Adds a PATCH variable for this specific run
         --  Note that is variable will override all previously defined
         --  project variables

         Run_Vars.Insert
           (Variables.Variable'(Name => Variables.Name_Utils.Value ("PATCH"),
                                Value => +Directories.Compose
                                  (Savadur.Config.Patch_Directory,
                                   Patch)));
      end if;

      For_All_Ref_Actions : declare
         use Savadur.Actions.Vectors;
         Is_Init     : Boolean := False;
         Position    : Cursor  := Selected_Scenario.Actions.First;
         Project_SCM : Savadur.SCM.SCM;
         pragma Unreferenced (Project_SCM);
      begin

         if Project.SCM_Id /= SCM.Null_Id then
            Project_SCM := Savadur.SCM.Keys.Element
              (Container => Savadur.Config.SCM.Configurations,
               Key       => Project.SCM_Id);
         end if;

         --  Check first if the source directory exists

         if not Directories.Exists (-Sources_Directory) then
            --  No sources directory. This means that the project has not been
            --  initialized.
            --  The sources directory has to be created by the SCM Call SCM
            --  init from current directory. If there is no SCM defined for
            --  this project we create the directory.

            Logs.Write ("Create directory : " & (-Sources_Directory));

            Is_Init := True;

            if Project.SCM_Id = SCM.Null_Id then
               Directories.Create_Directory (-Sources_Directory);

            else
               Run_SCM_Init : declare
                  Return_Code : Integer;
                  Result      : Boolean;
               begin

                  if Savadur.Config.Client_Server then

                     --  Notify the server that the action is starting

                     Notify_Start : declare
                        Server_URL : constant String :=
                                       Servers.URL (Servers.Get (Server));
                     begin
                        Client_Service.Client.Status_Start
                          (Key          => Config.Client.Get_Key,
                           Project_Name =>
                             Projects.Id_Utils.To_String (Project.Project_Id),
                           Scenario     => Scenarios.Id_Utils.To_String (Id),
                           Action       =>
                             Actions.Id_Utils.To_String (SCM.Init.Id),
                           Job_Id       => Job_Id,
                           Endpoint     => Server_URL);
                     end Notify_Start;
                  end if;

                  Execute
                    (Exec_Action   => Get_Action
                       (Project    => Project.all,
                        Ref_Action => Savadur.SCM.Init,
                        Vars       => Run_Vars),
                     Directory     =>
                     Projects.Project_Directory (Project),
                     Log_Filename  => Log_Filename
                       (Project, SCM.Init.Id, Job_Id),
                     Return_Code   => Return_Code,
                     Result        => Result);

                  if not Result or else Return_Code /= 0
                    or else not Directories.Exists (-Sources_Directory)
                  then
                     Status := Scenarios.Failure;
                     Send_Status (Server, Savadur.SCM.Init.Id);
                     raise Command_Parse_Error with "SCM init failed !";
                  end if;

                  if Savadur.Config.Client_Server then
                     Send_Status
                       (Server_Name => Server,
                        Action_Id   => Savadur.SCM.Init.Id,
                        Log_File    => Log_Filename (Project   => Project,
                                                     Action_Id => SCM.Init.Id,
                                                     Job_Id    => Job_Id));
                  end if;
               end Run_SCM_Init;
            end if;
         end if;

         if Selected_Scenario.Use_Tmp then
            Utils.Copy_Tree (-Sources_Directory, -Sources_Directory & "tmp");

            --  Change source directory to the new temporary copy
            Sources_Directory := Sources_Directory & "tmp";
         end if;

         --  Now run all actions

         Run_Actions : while Has_Element (Position) loop

            Execute_Command : declare
               use type Actions.Ref_Action;
               use type Scenarios.Run_Status;
               Ref         : constant Actions.Ref_Action := Element (Position);
               Log_File    : constant String :=
                               Log_Filename (Project, Ref.Id, Job_Id);
               Exec_Action : constant Actions.Action :=
                               Get_Action (Project    => Project.all,
                                           Ref_Action => Ref,
                                           Vars       => Run_Vars);
               Return_Code : Integer;
               Result      : Boolean;
            begin
               Last_Log := +Log_File;

               if not Is_Init or else not Exec_Action.Skip_On_Init then
                  if Savadur.Config.Client_Server then
                     --  Notify the server that the action is starting

                     Notify_Start_Action : declare
                        Server_URL : constant String :=
                                       Servers.URL (Servers.Get (Server));
                     begin
                        Client_Service.Client.Status_Start
                          (Key          => Config.Client.Get_Key,
                           Project_Name =>
                             Projects.Id_Utils.To_String (Project.Project_Id),
                           Scenario     => Scenarios.Id_Utils.To_String (Id),
                           Action       => Actions.Id_Utils.To_String (Ref.Id),
                           Job_Id       => Job_Id,
                           Endpoint     => Server_URL);
                     end Notify_Start_Action;
                  end if;

                  Execute (Exec_Action  => Exec_Action,
                           Directory    => -Sources_Directory,
                           Log_Filename => Log_File,
                           Return_Code  => Return_Code,
                           Result       => Result);

                  if not Result then
                     Status := Scenarios.Failure; --  Exit with error

                     if Savadur.Config.Client_Server then
                        Send_Status (Server, Ref.Id);
                     end if;

                     exit Run_Actions;
                  end if;

                  Status := Check (Project     => Project,
                                   Exec_Action => Exec_Action,
                                   Ref         => Ref,
                                   Return_Code => Return_Code,
                                   Job_Id      => Job_Id,
                                   Log_File    => Log_File,
                                   Diff_Data   => Diff_Data'Access);

                  if Savadur.Config.Client_Server then
                     Send_Status (Server_Name => Server,
                                  Action_Id   => Ref.Id,
                                  Log_File    => Log_File);
                  end if;

                  if Status = Scenarios.Failure then
                     case Ref.On_Error is
                        when Actions.Quit =>
                           Status := Scenarios.Skipped;
                           exit Run_Actions;

                        when Actions.Error =>
                           Project.Variables.Insert
                             (New_Item =>
                              Variables.Variable'
                                (Name  =>
                                 Variables.Name_Utils.Value ("failed_action"),
                                 Value => Actions.Id_Utils.
                                   To_Unbounded_String (Ref.Id)));
                           exit Run_Actions;

                        when Actions.Continue =>
                           null;
                     end case;
                  end if;
               end if;

               Next (Position);
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

      if Savadur.Config.Client_Server then
         --  Final server notification, pass the last log to this routine. This
         --  is important to be able to send log via e-mail for example.

         Send_Status (Server, Actions.End_Action.Id, -Last_Log);
      end if;

      --  Execute notifications hooks

      Build.Notification.Notify (Project, Status, Job_Id);

      return Status;
   end Run;

end Savadur.Build;
