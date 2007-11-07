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
with Ada.Text_IO;

with GNAT.OS_Lib;

with Savadur.SCM;
with Savadur.Config.SCM;

-------------------
-- Savadur.Build --
-------------------

package body Savadur.Build is

   use Ada;
   use Ada.Strings.Unbounded;

   function Programme_Name (Command : Savadur.Action.Command) return String;
   --  Returns the programme name from a string

   function Get_Command
     (Project    : Savadur.Config.Project.Project_Config;
      Ref_Action : Savadur.Action.Ref_Action)
     return Savadur.Action.Command;
   --  Returns the command matching the ref_action

   -------------
   -- Execute --
   -------------

   function Execute (Command : Savadur.Action.Command) return Boolean is
      use GNAT.OS_Lib;
      Prog_Name       : constant String := Programme_Name (Command);
      Result          : Boolean := False;
      Argument_String : Argument_List_Access;
   begin
      Argument_String := Argument_String_To_List
        (To_String (Unbounded_String (Command)));

      if Prog_Name = "make" or Prog_Name = "git-pull" then
         Result := True;
      else
         Ada.Text_IO.Put_Line ("{" & Prog_Name & "}");
      end if;

      Free (Argument_String);

      return Result;
   end Execute;

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
            Get_Action := Savadur.Action.Maps.Element
              (Container => SCM_Used.Actions,
               Key       => Action_Id);
         end;
      else
         --  Search in project action

         Get_Action := Maps.Element (Container => Project.Actions,
                                     Key       => Action_Id);
      end if;
      return Get_Action.Cmd;
   end Get_Command;

   --------------------
   -- Programme_Name --
   --------------------

   function Programme_Name (Command : Savadur.Action.Command) return String is
      Command_String : constant String :=
                         To_String (Unbounded_String (Command));
   begin
      for K in Command_String'Range loop
         if Command_String (K) = ' ' then
            return Command_String
              (Command_String'First .. Command_String'First + K - 2);
         end if;
      end loop;

      return Command_String;
   end Programme_Name;

   ---------
   -- Run --
   ---------

   function Run
     (Project : Savadur.Config.Project.Project_Config;
      Id      : Savadur.Scenario.Id)
      return Boolean
   is
      Selected_Scenario : Savadur.Scenario.Scenario;
      Result            : Boolean := True;
   begin
      Get_Selected_Scenario : begin
         Selected_Scenario := Project.Scenari.Element (Key => Id);
      exception
         when Constraint_Error =>
            raise Command_Parse_Error with " Scenario "
              & String (Id) & " not found";
      end Get_Selected_Scenario;

      For_All_Ref_Actions : declare
         use Savadur.Action;
         use Savadur.Action.Vectors;
         Position : Cursor := Selected_Scenario.Actions.First;
      begin
         while Has_Element (Position) loop

            Execute_Command : declare
               Cmd : Savadur.Action.Command :=
                       Get_Command (Project    => Project,
                                    Ref_Action => Element (Position));
            begin
               Result := Execute (Cmd);
            end Execute_Command;
            Next (Position);
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
