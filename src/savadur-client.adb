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

with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

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
   Scenario_Id          : Unbounded_String
     := Scenarios.Id_Utils.To_Unbounded_String (Scenarios.Default_Scenario);

   procedure Usage (Error_Message : in String := "");
   --  Display Usage

   -----------
   -- Usage --
   -----------

   procedure Usage (Error_Message : in String := "") is
   begin

      --  Display error message if not null

      if Error_Message /= "" then
         Logs.Write (Content => Error_Message, Kind => Logs.Error);

         --  Set exit status to Failure
         Command_Line.Set_Exit_Status (Command_Line.Failure);
      end if;

      Logs.Write ("usage : savadur-client [OPTIONS] -p|-project name"
                  & " -s|-sid scenario_id");
      Logs.Write ("OPTIONS :");
      Logs.Write ("    -savadurdir dirname : set savadur directory");
      Logs.Write ("          ($SAVADUR_DIR or $HOME/.savadur by default)");
      Logs.Write ("    -V|-verbose");
      Logs.Write ("    -VV|-very_verbose");
   end Usage;

begin

   if Command_Line.Argument_Count = 0 then
      Usage (Error_Message => "no argument ?");
      return;
   end if;

   Parse_Opt : begin
      Interate_On_Opt : loop
         case GNAT.Command_Line.Getopt
              ("V verbose VV very_verbose p: project: savadurdir: s: sid: ") is
            when ASCII.NUL =>
               exit Interate_On_Opt;

            when 'p' =>
               Complete_P : declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "project" or else Full = "p" then
                     Project_Name :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
                  end if;
               end Complete_P;

            when 's' =>
               Complete_S : declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "savadurdir" then
                     Config.Set_Savadur_Directory
                       (GNAT.Command_Line.Parameter);
                  elsif  Full = "sid" or else Full = "s" then
                     Scenario_Id :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
                  end if;
               end Complete_S;

            when 'v' | 'V' =>
               Complete_V : declare
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
               end Complete_V;

            when others =>
               Usage (Error_Message => "unknown syntax");
               return;
         end case;
      end loop Interate_On_Opt;
   exception
      when GNAT.Command_Line.Invalid_Section
         | GNAT.Command_Line.Invalid_Switch
         | GNAT.Command_Line.Invalid_Parameter =>
         raise Syntax_Error with "unknown syntax";
   end Parse_Opt;

   if To_String (Project_Name) = "" then
      Usage (Error_Message => "no project name");
      return;
   end if;

   --  Parse SCM configuration files

   Savadur.Config.SCM.Parse;

   Run_Project : declare
      Project : aliased Config.Project.Project_Config :=
                  Config.Project.Parse (-Project_Name);
      Env_Var : Environment_Variables.Maps.Map;
   begin

      Logs.Write (Content => "Savadur client" & ASCII.LF,
                  Kind    => Logs.Verbose);

      Logs.Write (Content => "SCM : " & ASCII.LF
                  & To_String (Unbounded_String (Project.SCM_Id)) & ASCII.LF,
                  Kind    => Logs.Very_Verbose);

      Logs.Write (Content => "Action list : " & ASCII.LF
                  & Actions.Image (Project.Actions) & ASCII.LF,
                  Kind    => Logs.Very_Verbose);

      Logs.Write (Content => "Scenarios : " & ASCII.LF
                  & Scenarios.Image (Project.Scenarios) & ASCII.LF,
                  Kind    => Logs.Very_Verbose);

      Logs.Write (Content => "SCM Found" & ASCII.LF
                  & Savadur.SCM.Image (Savadur.Config.SCM.Configurations)
                  & ASCII.LF,
                  Kind    => Logs.Very_Verbose);

      Env_Var := Savadur.Config.Environment_Variables.Parse (Project'Access);
      --  ??? Should be called by Run

      if Savadur.Build.Run
        (Project => Project'Access,
         Env_Var => Env_Var,
         Id      => Scenarios.Id (Scenario_Id))
      then
         Logs.Write ("Success");
      else
         Logs.Write ("Failure");
      end if;
   end Run_Project;

exception
   when E : Syntax_Error
      | Savadur.Config.Config_Error
      | Savadur.Config.Project.Config_Error
      | Savadur.Config.Environment_Variables.Config_Error =>
      Usage (Error_Message => Exceptions.Exception_Message (E));
end Savadur.Client;
