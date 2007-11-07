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
     (Command         : in Savadur.Action.Command;
      Prog_Name       : out Unbounded_String;
      Argument_String : out OS_Lib.Argument_List_Access);
   --  Returns the programme name and the argument list

   function Get_Command
     (Project    : Savadur.Config.Project.Project_Config;
      Ref_Action : Savadur.Action.Ref_Action)
     return Savadur.Action.Command;
   --  Returns the command matching the ref_action

   function Parse
     (Project : Savadur.Config.Project.Project_Config;
      Cmd     : Savadur.Action.Command)
     return Savadur.Action.Command;
   --  Replace strings beginning with $
   --  by the correponding entry in project <variable> section

   -------------
   -- Execute --
   -------------

   function Execute
     (Command   : Savadur.Action.Command;
      Directory : String)
      return Boolean
   is
      use GNAT.OS_Lib;
      Exec_Path       : OS_Lib.String_Access;
      Argument_String : Argument_List_Access;
      Prog_Name       : Unbounded_String;
      Result          : Boolean;
      PID             : Process_Id;
   begin

      Get_Arguments (Command, Prog_Name, Argument_String);

      --  Chdir to the requested base directory

      Directories.Set_Directory (Directory);

      Exec_Path := Locate_Exec_On_Path (-Prog_Name);

      if Exec_Path = null then
         raise Command_Parse_Error with "No Exec " & (-Prog_Name)
           & " on this system";
      end if;

      PID := Non_Blocking_Spawn
        (Program_Name => Exec_Path.all,
         Args         => Argument_String.all,
         Output_File  => "LOG" & (-Prog_Name),
         Err_To_Out   => True);

      Wait_Process (PID, Result);

      Free (Argument_String);
      Free (Exec_Path);

      return Result;
   end Execute;

   -------------------
   -- Get_Arguments --
   -------------------

   procedure Get_Arguments
     (Command         : in Savadur.Action.Command;
      Prog_Name       : out Unbounded_String;
      Argument_String : out OS_Lib.Argument_List_Access)
   is
      Command_String : constant String := -Unbounded_String (Command);
   begin
      for K in Command_String'Range loop
         if Command_String (K) = ' ' then
            Prog_Name := +Command_String (Command_String'First .. K - 1);
            Argument_String := OS_Lib.Argument_String_To_List
              (Command_String (K + 1 .. Command_String'Last));
            return;
         end if;
      end loop;

      Prog_Name := +Command_String;
   end Get_Arguments;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (Project    : Savadur.Config.Project.Project_Config;
      Ref_Action : Savadur.Action.Ref_Action)
      return Savadur.Action.Command
   is
      use Savadur.Action;
      Action_Id  : constant Id := -Ref_Action.Id;
      Get_Action : Savadur.Action.Action;
   begin

      if Ref_Action.Action_Type = Savadur.Action.SCM then
         --  Search the action in the SCM list

         declare
            use Savadur.SCM;
            SCM_Used : Savadur.SCM.SCM := Savadur.SCM.Maps.Element
              (Container => Savadur.Config.SCM.Configurations,
               Key       => -Project.SCM);
         begin

            Ada.Text_IO.Put_Line (Savadur.Action.Image (SCM_Used.Actions));
            Get_Action := Savadur.Action.Maps.Element
              (Container => SCM_Used.Actions,
               Key       => Action_Id);

         end;
      else
         --  Search in project action

         Get_Action := Maps.Element (Container => Project.Actions,
                                     Key       => Action_Id);
      end if;

      return Parse (Project, Get_Action.Cmd);
   end Get_Command;

   -----------
   -- Parse --
   -----------

   function Parse
     (Project : Savadur.Config.Project.Project_Config;
      Cmd     : Savadur.Action.Command)
      return Savadur.Action.Command
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
               Append (Result, Savadur.Config.Project.
                                Var_Maps.Element (Project.Variable, Key));
               Start      := K;
               Do_Replace := False;
            end Query_Project_Variables;
         end if;
      end loop;

      if Do_Replace then
         Append
           (Result,
            Project.Variable.Element (Source (Start + 1 .. Source'Last)));
      else
         Append (Result, Source (Start .. Source'Last));
      end if;

      return Savadur.Action.Command (Result);
   exception
      when E : others => Text_IO.Put_Line (Exception_Information (E));
         raise;
   end Parse;

   ---------
   -- Run --
   ---------

   function Run
     (Project : Savadur.Config.Project.Project_Config;
      Id      : Savadur.Scenario.Id)
      return Boolean
   is
      Selected_Scenario : Savadur.Scenario.Scenario;
      Sources_Directory : Unbounded_String;
      Result            : Boolean := True;
   begin
      Get_Selected_Scenario : begin
         Selected_Scenario := Project.Scenari.Element (Key => Id);
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with " Scenario "
              & String (Id) & " not found";
      end Get_Selected_Scenario;

      Get_Sources_Directory : begin
         Sources_Directory := +(Project.Variable.Element ("sources"));
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with " No sources directory !";
      end Get_Sources_Directory;

      For_All_Ref_Actions : declare
         use Savadur.Action;
         use Savadur.Action.Vectors;
         Position : Cursor := Selected_Scenario.Actions.First;
      begin
         while Has_Element (Position) loop

            Execute_Command : declare
               Ref : Ref_Action := Element (Position);
               Cmd : Savadur.Action.Command :=
                       Get_Command (Project    => Project,
                                    Ref_Action => Ref);
            begin
               if Directories.Exists (-Sources_Directory) then
                  Result := Execute (Cmd, -Sources_Directory);
                  Next (Position);
               else
                  --  No sources directory. This means that the project has not
                  --  been initialized.
                  --  The sources directory has to be created by the SCM
                  --  Call SCM init from current directory

                  Ada.Text_IO.Put_Line
                    ("Create directory" & (-Sources_Directory));

                  Result := Execute
                    (Get_Command (Project    => Project,
                                  Ref_Action => Savadur.SCM.SCM_Init),
                     Directories.Current_Directory);

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
