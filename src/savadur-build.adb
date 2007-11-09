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

with GNAT.OS_Lib;

with Savadur.Utils;
with Savadur.SCM;
with Savadur.Config.SCM;
with Savadur.Variables;
with Ada.Text_IO;

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

   -------------
   -- Execute --
   -------------

   function Execute
     (Exec_Action   : in Actions.Action;
      Check_Value   : in String;
      Directory     : in String;
      Log_Filename  : in String)
      return Boolean
   is
      use GNAT.OS_Lib;
      use type Actions.Result_Type;
      Exec_Path       : OS_Lib.String_Access;
      Argument_String : Argument_List_Access;
      Prog_Name       : Unbounded_String;
      Result          : Boolean;
      Return_Code     : Integer;
   begin

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

      Result := False;

      Check_Return_Value : begin
         if Exec_Action.Result = Actions.Exit_Status
           and then Return_Code = Integer'Value (Check_Value)
         then
            Result := True;
         end if;
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with "Value " & Check_Value
              & " is not an exit status";
      end Check_Return_Value;

      Free (Argument_String);
      Free (Exec_Path);

      return Result;
   exception
      when E : others => Text_IO.Put_Line (Exception_Information (E));
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
      Action_Id  : constant Id := -Ref_Action.Id;
      Get_Action : Action;
   begin

      if Ref_Action.Action_Type = Actions.SCM then
         --  Search the action in the SCM list

         declare
            use Savadur.SCM;
            SCM_Used : Savadur.SCM.SCM := Savadur.SCM.Maps.Element
              (Container => Savadur.Config.SCM.Configurations,
               Key       => -Project.SCM_Id);
         begin

            Ada.Text_IO.Put_Line (Image (SCM_Used.Actions));
            Get_Action := Actions.Maps.Element
              (Container => SCM_Used.Actions,
               Key       => Action_Id);

         end;
      else
         --  Search in project action

         Get_Action := Maps.Element (Container => Project.Actions,
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
         declare
            Empty_String : constant OS_Lib.String_Access := new String'("");
         begin
            Argument_String :=
              new Argument_List'(1 => Empty_String);
         end;
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
               Key : constant String := Source (Start + 1 .. K - 1);
            begin
               Append (Result, Savadur.Variables.Maps.
                         Element (Project.Variables, Key));
               Start      := K;
               Do_Replace := False;
            end Query_Project_Variables;
         end if;
      end loop;

      if Do_Replace then
         Append
           (Result,
            Project.Variables.Element (Source (Start + 1 .. Source'Last)));
      else
         Append (Result, Source (Start .. Source'Last));
      end if;

      return Actions.Command (Result);
   exception
      when E : others => Text_IO.Put_Line (Exception_Information (E));
         raise;
   end Parse;

   ---------
   -- Run --
   ---------

   function Run
     (Project : Config.Project.Project_Config;
      Id      : Scenarios.Id)
      return Boolean
   is
      Selected_Scenario : Scenarios.Scenario;
      Sources_Directory : Unbounded_String;
      Work_Directory    : Unbounded_String;
      Project_Directory : Unbounded_String;
      Log_Directory     : Unbounded_String;
      Result            : Boolean := True;
   begin
      Get_Selected_Scenario : begin
         Selected_Scenario := Project.Scenarios.Element (Key => Id);
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with " Scenario "
              & String (Id) & " not found";
      end Get_Selected_Scenario;

      Work_Directory := +Directories.Compose
        (Containing_Directory => Config.Savadur_Directory,
         Name                 => "work");

      if not Directories.Exists (Name => -Work_Directory) then
         Directories.Create_Path (New_Directory => -Work_Directory);
      end if;

      Project_Directory := +Directories.Compose
        (Containing_Directory => -Work_Directory,
         Name                 => -Unbounded_String (Project.Project_Id));

      Log_Directory := +Directories.Compose
        (Containing_Directory => -Project_Directory,
         Name                 => "log");

      if not Directories.Exists (Name => -Log_Directory) then
         Directories.Create_Path (New_Directory => -Log_Directory);
      end if;

      Get_Sources_Directory : begin
         Sources_Directory := +Directories.Compose
           (Containing_Directory => -Project_Directory,
            Name                 => Project.Variables.Element ("sources"));
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with " No sources directory !";
      end Get_Sources_Directory;

      For_All_Ref_Actions : declare
         use Savadur.Actions;
         use Savadur.Actions.Vectors;
         Position : Cursor := Selected_Scenario.Actions.First;
      begin
         while Has_Element (Position) loop

            Execute_Command : declare
               Ref         : constant Ref_Action := Element (Position);
               Log_File    : constant String     := Directories.Compose
                 (Containing_Directory => -Log_Directory,
                  Name                 => "LOG_" & String (-Ref.Id));
               Exec_Action : constant Action    :=
                               Get_Action (Project    => Project,
                                           Ref_Action => Ref);
            begin
               if Directories.Exists (-Sources_Directory) then
                  Result := Execute (Exec_Action  => Exec_Action,
                                     Check_Value  => -Ref.Value,
                                     Directory    => -Sources_Directory,
                                     Log_Filename => Log_File);
                  Next (Position);
               else
                  --  No sources directory. This means that the project has not
                  --  been initialized.
                  --  The sources directory has to be created by the SCM
                  --  Call SCM init from current directory

                  Ada.Text_IO.Put_Line
                    ("Create directory : " & (-Sources_Directory));

                  Result := Execute
                    (Exec_Action   => Get_Action
                       (Project    => Project,
                        Ref_Action => Savadur.SCM.SCM_Init),
                     Check_Value   => "0",
                     Directory     => -Project_Directory,
                     Log_Filename  => Directories.Compose
                       (Containing_Directory => -Log_Directory,
                        Name                 => "LOG_init"));

                  if not Directories.Exists (-Sources_Directory) then
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
                 & String (-Element (Position).Id) & " not found";
            else
               raise Command_Parse_Error with " Command not found";
            end if;
      end For_All_Ref_Actions;
      return Result;
   end Run;

end Savadur.Build;
