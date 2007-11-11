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
--  savadur-client [OPTIONS] -project name -sid scenario_id
--
--  OPTIONS :
--       -savadurdir dirname : Set savadur directory
--                             ($SAVADUR_DIR or $HOME / .savadur by default)
--       -verbose
--       -very_verbose

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
with Savadur.Logs;
with Savadur.Environment_Variables;

procedure Savadur.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Syntax_Error         : exception;

   Project_Name         : Unbounded_String;
   Project_Filename     : Unbounded_String;
   Project_Env_Filename : Unbounded_String;
   Scenario_Id          : Unbounded_String
     := Scenarios.Id_Utils.To_Unbounded_String (Scenarios.Default_Scenario);

   procedure Usage;
   --  Display Usage

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Logs.Write ("usage : savadur-client [OPTIONS] -p|-project name"
                  & " -s|-sid scenario_id");
      Logs.Write ("OPTIONS :");
      Logs.Write ("    --savadurdir dirname : set savadur directory");
      Logs.Write ("          ($SAVADUR_DIR or $HOME/.savadur by default)");
      Logs.Write ("    -V|-verbose");
      Logs.Write ("    -VV|-very_verbose");
   end Usage;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      raise Syntax_Error with "no argument ?";
   else
      Getopt : begin
         loop
            case GNAT.Command_Line.Getopt
              ("V verbose VV very_verbose p: project: savadurdir: s: sid: ") is
               when ASCII.NUL =>
                  exit;

               when 'p' =>
                  declare
                     Full : constant String := GNAT.Command_Line.Full_Switch;
                  begin
                     if Full = "project" or else Full = "p" then
                        Project_Name :=
                          To_Unbounded_String (GNAT.Command_Line.Parameter);
                     end if;
                  end;

               when 's' =>
                  declare
                     Full : constant String := GNAT.Command_Line.Full_Switch;
                  begin
                     if Full = "savadurdir" then
                        Config.Set_Savadur_Directory
                          (GNAT.Command_Line.Parameter);
                     elsif  Full = "sid" or else Full = "s" then
                        Scenario_Id :=
                          To_Unbounded_String (GNAT.Command_Line.Parameter);
                     end if;
                  end;

               when 'v' | 'V' =>
                  declare
                     Full : constant String := GNAT.Command_Line.Full_Switch;
                  begin
                     if Full = "verbose" or else Full = "V"  then
                        Logs.Set (Kind => Logs.Verbose, Activated => True);
                     elsif Full = "very_verbose" or else Full = "VV" then
                        Logs.Set (Kind      => Logs.Verbose,
                                  Activated => True);
                        Logs.Set (Kind      => Logs.Very_Verbose,
                                  Activated => True);
                     end if;
                  end;

               when others =>
                  raise Syntax_Error with "unknown syntax";
            end case;
         end loop;
      exception
         when GNAT.Command_Line.Invalid_Section
            | GNAT.Command_Line.Invalid_Switch
            | GNAT.Command_Line.Invalid_Parameter =>
            raise Syntax_Error with "unknown syntax";
      end Getopt;
   end if;

   if To_String (Project_Name) = "" then
      raise Syntax_Error with "no project name";
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

      Logs.Write ("Savadur client" & ASCII.LF, Logs.Verbose);
      Logs.Write ("SCM : " & ASCII.LF
                  & To_String (Unbounded_String (Project.SCM_Id)) & ASCII.LF,
                  Logs.Very_Verbose);
      Logs.Write ("Action list : " & ASCII.LF
                  & Actions.Image (Project.Actions) & ASCII.LF,
                  Logs.Very_Verbose);
      Logs.Write ("Scenarios : " & ASCII.LF
                  & Scenarios.Image (Project.Scenarios) & ASCII.LF,
                  Logs.Very_Verbose);
      Logs.Write ("SCM Found" & ASCII.LF
                  & Savadur.SCM.Image (Savadur.Config.SCM.Configurations)
                  & ASCII.LF,
                  Logs.Very_Verbose);

      if Directories.Exists (-Project_Env_Filename) then
         Env_Var := Savadur.Config.Environment_Variables.Parse
           (Filename => -Project_Env_Filename);
      end if;

      if Savadur.Build.Run
        (Project => Project,
         Env_Var => Env_Var,
         Id      => Scenarios.Id (Scenario_Id))
      then
         Logs.Write ("Success");
      else
         Logs.Write ("Failure");
      end if;
   end;

exception
   when E : Syntax_Error | Savadur.Config.Config_Error  =>
      Logs.Write (Exceptions.Exception_Message (E), Logs.Error);
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Savadur.Client;
