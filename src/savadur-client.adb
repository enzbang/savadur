------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
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

with Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Directories;

with GNAT.Command_Line;

with Savadur.Actions;
with Savadur.Build;
with Savadur.Projects;
with Savadur.Client_Service.Client;
with Savadur.Client_Service.Types;
with Savadur.Config.Environment_Variables;
with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Config.Server;
with Savadur.Environment_Variables;
with Savadur.Jobs;
with Savadur.Logs;
with Savadur.Server_Service;
with Savadur.Scenarios;
with Savadur.SCM;
with Savadur.Servers;
with Savadur.Utils;
with Savadur.Version;
with Savadur.Web.Client;

procedure Savadur.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Syntax_Error         : exception;

   Project_Name         : Unbounded_String;
   Scenario_Id          : Unbounded_String
     := Scenarios.Id_Utils.To_Unbounded_String (Scenarios.Default_Scenario);

   New_Server           : Savadur.Servers.Server;

   procedure Usage (Error_Message : in String := "");
   --  Displays Usage

   procedure Run_Standalone;
   --  Launches client in standalone mode

   procedure Run_Server_Mode;
   --  Launches client in server mode

   procedure Add_Remote_Server;
   --  Add a new remote server

   procedure List_Remote_Server;
   --  Lists remote servers

   Action               : access procedure;
   --  Action to execute (after Command Line parsing)

   procedure Add_Remote_Server is
      use Ada.Text_IO;
      File : File_Type;
      Filename : constant String :=
                   Ada.Directories.Compose
                     (Containing_Directory => Config.Server_Directory,
                      Name                 => -New_Server.Name,
                      Extension            => "xml");
   begin
      Create (File => File,
              Mode => Out_File,
              Name => Filename);

      Logs.Write ("Add new remote server : "
                  & (-New_Server.Name) & " " & (-New_Server.URL));

      Put_Line (File, "<server>");
      Put_Line (File, "<name value='" & (-New_Server.Name) & "'/>");
      Put_Line (File, "<location url='" & (-New_Server.URL) & "'/>");
      Put_Line (File, "</server>");

      Close (File);
   end Add_Remote_Server;

   ------------------------
   -- List_Remote_Server --
   ------------------------

   procedure List_Remote_Server is
      use type Containers.Count_Type;
   begin
      --  Parse the servers

      Config.Server.Parse;

      if Config.Server.Configurations.Length = 0 then
         Logs.Write
           (Content => "No server configured",
            Kind    => Logs.Error);

      else
         Logs.Write (Savadur.Servers.Image (Config.Server.Configurations));
      end if;
   end List_Remote_Server;

   ---------------------
   -- Run_Server_Mode --
   ---------------------

   procedure Run_Server_Mode is

      use type Containers.Count_Type;

      procedure Register (Cursor : in Servers.Sets.Cursor);
      --  Register client to the pointer server

      --------------
      -- Register --
      --------------

      procedure Register (Cursor : in Servers.Sets.Cursor) is
         Server   : constant Savadur.Servers.Server :=
                      Servers.Sets.Element (Cursor);
         Metadata : constant Client_Service.Types.Metadata_Type :=
                      (OS => +"windows");
      begin
         Logs.Write
           (Content =>
              "Register to " & (-Server.Name) & " at " & (-Server.URL),
            Kind    => Logs.Information);

         Client_Service.Client.Register
           ("me", Metadata, Server_Service.URL, -Server.URL);

         Logs.Write (Content => "Done.",
                     Kind    => Logs.Information);
      end Register;

   begin

      --  Parse SCM configuration files ???

      Savadur.Config.SCM.Parse;

      --  Parse the servers

      Config.Server.Parse;

      if Config.Server.Configurations.Length = 0 then
         Logs.Write
           (Content => "No server configured",
            Kind    => Logs.Error);

      else
         --  Start the server

         Web.Client.Start;

         --  Register this client to all known server

         Config.Server.Configurations.Iterate (Register'Access);
      end if;
   end Run_Server_Mode;

   --------------------
   -- Run_Standalone --
   --------------------

   procedure Run_Standalone is
      --  ??? This part should be moved into Savadur.Jobs
   begin
      --  Parse SCM configuration files

      Savadur.Config.SCM.Parse;

      if To_String (Project_Name) = "" then
         Usage (Error_Message => "no project name");
         return;
      end if;

      Run_Project : declare
         Project : aliased Projects.Project_Config :=
                     Config.Project.Parse (-Project_Name);
         Env_Var : Environment_Variables.Maps.Map;
      begin
         Logs.Write
           (Content => "Savadur client" & ASCII.LF,
            Kind    => Logs.Verbose);

         Logs.Write
           (Content => "SCM : " & ASCII.LF
            & To_String (Unbounded_String (Project.SCM_Id)) & ASCII.LF,
            Kind    => Logs.Very_Verbose);

         Logs.Write
           (Content => "Action list : " & ASCII.LF
            & Actions.Image (Project.Actions) & ASCII.LF,
            Kind    => Logs.Very_Verbose);

         Logs.Write
           (Content => "Scenarios : " & ASCII.LF
            & Scenarios.Image (Project.Scenarios) & ASCII.LF,
            Kind    => Logs.Very_Verbose);

         Logs.Write
           (Content => "SCM Found" & ASCII.LF
            & Savadur.SCM.Image (Savadur.Config.SCM.Configurations)
            & ASCII.LF,
            Kind    => Logs.Very_Verbose);

         Env_Var :=
           Savadur.Config.Environment_Variables.Parse (Project'Access);
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

      Jobs.Stop;
   end Run_Standalone;

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

      Logs.Write ("Savadur " & Version.Simple);
      Logs.Write ("usage : savadur-client [OPTIONS] -p|-project name"
                  & " -s|-sid scenario_id");
      Logs.Write ("OPTIONS :");
      Logs.Write ("    -savadurdir dirname : set savadur directory");
      Logs.Write ("          ($SAVADUR_DIR or $HOME/.savadur by default)");
      Logs.Write ("    -v|-version");
      Logs.Write ("    -V|-verbose");
      Logs.Write ("    -VV|-very_verbose");
      Logs.Write ("    -server             : run in server mode");
      Logs.Write ("    -remotelist        : List new remote server");
      Logs.Write ("    -remoteadd         : Add a new remote server");
   end Usage;

begin

   if Command_Line.Argument_Count = 0 then
      Usage (Error_Message => "no argument ?");
      return;
   end if;

   GNAT.Command_Line.Initialize_Option_Scan (Section_Delimiters => "remote");

   Interate_On_Opt : loop
      case GNAT.Command_Line.Getopt
           ("V verbose VV very_verbose version v "
            & "p: project: savadurdir: s: sid: server")
         is
         when ASCII.NUL =>
            exit Interate_On_Opt;

            when 'p' =>
            Complete_P : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "project" or else Full = "p" then
                  Project_Name :=
                    To_Unbounded_String (GNAT.Command_Line.Parameter);
                  Logs.Write (GNAT.Command_Line.Parameter);
                  Action := Run_Standalone'Access;

               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_P;

         when 's' =>
            Complete_S : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "savadurdir" then
                  Config.Set_Savadur_Directory
                    (GNAT.Command_Line.Parameter);

               elsif Full = "sid" or else Full = "s" then
                  Scenario_Id :=
                    To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif Full = "server" then
                  Action := Run_Server_Mode'Access;
               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_S;

         when 'r' =>
            Complete_R : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "remoteadd" then
                  Action := List_Remote_Server'Access;
               else
                  Action := Add_Remote_Server'Access;
               end if;
            end Complete_R;
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

               elsif Full = "version" or else Full = "v" then
                  Logs.Write (Content => "Savadur "
                              & Savadur.Version.Complete);
                  return;

               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_V;

         when others =>
            Usage (Error_Message => "unknown syntax");
            return;
      end case;
   end loop Interate_On_Opt;

   GNAT.Command_Line.Goto_Section ("remote");
   Remote_Opt : loop
      case GNAT.Command_Line.Getopt ("* add list") is
         when ASCII.NUL =>
            exit Remote_Opt;

         when '*' =>
            if New_Server.Name = Null_Unbounded_String then
               New_Server.Name := +GNAT.Command_Line.Full_Switch;
            else
               New_Server.URL  := +GNAT.Command_Line.Full_Switch;
            end if;

         when 'a' =>
            Action := Add_Remote_Server'Access;

         when 'l' =>
            Action := List_Remote_Server'Access;

         when others =>
            Usage (Error_Message => "unknown syntax");
            return;

      end case;
   end loop Remote_Opt;

   if Action /= null then
      Action.all;
   else
      Usage;
   end if;

exception
   when GNAT.Command_Line.Invalid_Section
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter =>
      Usage ("unknown syntax");
   when E : Savadur.Config.Config_Error
      | Savadur.Config.Project.Config_Error
      | Savadur.Config.Environment_Variables.Config_Error =>
      Usage (Error_Message => Exceptions.Exception_Message (E));
end Savadur.Client;
