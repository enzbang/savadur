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
--  savadur-client [ --savadur-dir dirname] -project filename -sid scenario_id
--

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

with GNAT.Command_Line;

with Savadur.Utils;
with Savadur.Config.Project;
with Savadur.Build;
with Savadur.Config.SCM;
with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

procedure Savadur.Client is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Syntax_Error     : exception;

   Project_Filename : Unbounded_String;
   SCM_Dir          : Unbounded_String;
   Scenario_Id      : Unbounded_String :=
                        +String (Savadur.Scenario.Default_Scenario);

   procedure Usage;
   --  Display Usage

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line
        ("usage: savadur-client  -savadur-dir dirname"
         & "-project filename - mode modename");
   end Usage;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      raise Syntax_Error;
   else
      loop
         case GNAT.Command_Line.Getopt ("project: configdir: sid: ") is
            when ASCII.NUL =>
               exit;

            when 'p' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "project" then
                     Project_Filename :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
                  else
                     raise Syntax_Error;
                  end if;
               end;
            when 's' =>
               declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "savadur-dir" then
                     SCM_Dir :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
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

   if To_String (Project_Filename) = "" then
      raise Syntax_Error;
   end if;

   --  Parse SCM configuration files

   Savadur.Config.SCM.Parse;

   declare
      Project : Savadur.Config.Project.Project_Config :=
                  Savadur.Config.Project.Parse (To_String (Project_Filename));
   begin

      Put_Line ("Savadur client");
      New_Line;
      Put_Line ("SCM : " & To_String (Unbounded_String (Project.SCM_Id)));
      New_Line;
      Put_Line ("Action list : ");
      New_Line;
      Put_Line (Savadur.Action.Image (Project.Actions));
      New_Line;
      Put_Line ("Scenari : ");
      New_Line;
      Put_Line (Savadur.Scenario.Image (Project.Scenari));
      New_Line;
      New_Line;
      New_Line;
      Put_Line ("SCM Found");
      New_Line;
      Put_Line (Savadur.SCM.Image (Savadur.Config.SCM.Configurations));

      if Savadur.Build.Run
        (Project,
         Savadur.Scenario.Id (To_String (Scenario_Id))) then
         Put_Line ("Success");
      else
         Put_Line ("Failure");
      end if;
   end;

exception
   when Syntax_Error | GNAT.Command_Line.Invalid_Switch  =>
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Savadur.Client;
