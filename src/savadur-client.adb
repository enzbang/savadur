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

--
--  Usage :
--
--  savadur-client [ --savadurdir dirname] -project name -sid scenario_id
--

with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Directories;

with GNAT.Command_Line;

with Savadur.Utils;
with Savadur.Config.Project;
with Savadur.Build;
with Savadur.Config.SCM;
with Savadur.Config.Environment_Variables;
with Savadur.Actions;
with Savadur.Scenarios;
with Savadur.SCM;
with Savadur.Environment_Variables;

procedure Savadur.Client is

   use Ada;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Syntax_Error         : exception;

   Project_Name         : Unbounded_String;
   Project_Filename     : Unbounded_String;
   Project_Env_Filename : Unbounded_String;
   Scenario_Id          : Unbounded_String :=
                            +String (Scenarios.Default_Scenario);

   procedure Usage;
   --  Display Usage

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line
        ("usage: savadur-client  -savadurdir dirname"
         & "   -project name   -mode modename");
   end Usage;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      raise Syntax_Error;
   else
      loop
         case GNAT.Command_Line.Getopt ("project: savadurdir: sid: ") is
            when ASCII.NUL =>
               exit;

            when 'p' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "project" then
                     Project_Name :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
                  else
                     raise Syntax_Error;
                  end if;
               end;
            when 's' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "savadurdir" then
                     Config.Set_Savadur_Directory
                       (GNAT.Command_Line.Parameter);
                  elsif  Full = "sid" then
                     Scenario_Id :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
                     raise Syntax_Error;
                  end if;
               end;
            when others =>
               raise Syntax_Error;
         end case;
      end loop;
   end if;

   if To_String (Project_Name) = "" then
      raise Syntax_Error;
   end if;

   --  Parse SCM configuration files

   Savadur.Config.SCM.Parse;

   Get_Project_Filename : declare
      Project_Directory : constant String := Directories.Compose
        (Containing_Directory => Config.Savadur_Directory,
         Name                 => "projects");
      Env_Var_Directory : constant String := Directories.Compose
        (Containing_Directory => Config.Savadur_Directory,
         Name                 => "env");
   begin
      Project_Filename := +Directories.Compose
        (Containing_Directory => Project_Directory,
         Name                 => -Project_Name,
         Extension            => "xml");

      Project_Env_Filename := +Directories.Compose
        (Containing_Directory => Env_Var_Directory,
         Name                 => -Project_Name,
         Extension            => "xml");
   exception
      when IO_Exceptions.Name_Error =>
         raise Syntax_Error with "Wrong project name !";
   end Get_Project_Filename;

   declare
      Project : constant Config.Project.Project_Config :=
                  Config.Project.Parse (-Project_Filename);
      Env_Var : Environment_Variables.Maps.Map;
   begin

      Put_Line ("Savadur client");
      New_Line;
      Put_Line ("SCM : " & To_String (Unbounded_String (Project.SCM_Id)));
      New_Line;
      Put_Line ("Action list : ");
      New_Line;
      Put_Line (Actions.Image (Project.Actions));
      New_Line;
      Put_Line ("Scenarios : ");
      New_Line;
      Put_Line (Scenarios.Image (Project.Scenarios));
      New_Line;
      New_Line;
      New_Line;
      Put_Line ("SCM Found");
      New_Line;
      Put_Line (Savadur.SCM.Image (Savadur.Config.SCM.Configurations));

      if Directories.Exists (-Project_Env_Filename) then
         Env_Var := Savadur.Config.Environment_Variables.Parse
           (Filename => -Project_Env_Filename);
      end if;

      if Savadur.Build.Run
        (Project => Project,
         Env_Var => Env_Var,
         Id      => Scenarios.Id (To_String (Scenario_Id)))
      then
         Put_Line ("Success");
      else
         Put_Line ("Failure");
      end if;
   end;

exception
   when E : Syntax_Error | GNAT.Command_Line.Invalid_Switch  =>
      Text_IO.Put_Line (Exceptions.Exception_Message (E));
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Savadur.Client;
