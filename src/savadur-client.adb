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

with Savadur.Client_Service.Client;
with Savadur.Client_Service.Types;
with Savadur.Config.Environment_Variables;
with Savadur.Config.Project;
with Savadur.Config.Client;
with Savadur.Config.SCM;
with Savadur.Config.Server;
with Savadur.Jobs.Client;
with Savadur.Logs;
with Savadur.Projects;
with Savadur.Scenarios;
with Savadur.Server_Service;
with Savadur.Signed_Files;
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

   Action               : access procedure;
   --  Action to execute after Command Line parsing

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

   -----------------------
   -- Add_Remote_Server --
   -----------------------

   procedure Add_Remote_Server is
      use Ada.Text_IO;
      File : File_Type;
      Filename : constant String :=
                   Directories.Compose
                     (Containing_Directory => Config.Server_Directory,
                      Name                 => -New_Server.Name,
                      Extension            => "xml");
   begin
      Create (File => File, Mode => Out_File, Name => Filename);

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
      --  Registers client to the pointer server

      --------------
      -- Register --
      --------------

      procedure Register (Cursor : in Servers.Sets.Cursor) is
         Server   : constant Savadur.Servers.Server :=
                      Servers.Sets.Element (Cursor);
         Metadata : constant Client_Service.Types.Metadata_Type :=
                      (OS => +"windows");
         Key      : constant String := Config.Client.Get_Key;
      begin
         Logs.Write
           (Content =>
              "Register to " & (-Server.Name) & " at " & (-Server.URL),
            Kind    => Logs.Information);

         Client_Service.Client.Register
           (Key, Metadata, Server_Service.URL, -Server.URL);

         Logs.Write (Content => "Done.",
                     Kind    => Logs.Information);
      end Register;

   begin
      --  Parse configuration files

      Config.SCM.Parse;
      Config.Server.Parse;
      Config.Project.Parse;

      Logs.Write
        (Content => "SCM Found" & ASCII.LF
         & Savadur.SCM.Image (Savadur.Config.SCM.Configurations)
         & ASCII.LF,
         Kind    => Logs.Very_Verbose);

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
   begin
      --  Parse SCM configuration files
      --  We do not load the servers description as we want to run in
      --  standalone mode. In this case the project and SCM won't get checked
      --  on the server side.

      Savadur.Config.SCM.Parse;
      Savadur.Config.Project.Parse;

      Logs.Write
        (Content => "SCM Found" & ASCII.LF
         & Savadur.SCM.Image (Savadur.Config.SCM.Configurations)
         & ASCII.LF,
         Kind    => Logs.Very_Verbose);

      if To_String (Project_Name) = "" then
         Usage (Error_Message => "no project name");
         return;
      end if;

      Run_Project : declare
         Project        : aliased Projects.Project_Config :=
                            Config.Project.Get (-Project_Name);
         Signed_Project : Signed_Files.Handler;
      begin
         Logs.Write
           (Content => "Savadur client" & ASCII.LF,
            Kind    => Logs.Verbose);

         Signed_Files.Create
           (Signed_Project,
            -Project_Name,
            Projects.Project_Filename (Project'Access));

         Jobs.Client.Queue.Add (Signed_Project, -Scenario_Id);
      end Run_Project;

      Jobs.Client.Queue.Stop;
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
      Logs.Write ("    -L filename         : use filename for log file");
      Logs.Write ("    -server             : run in server mode");
      Logs.Write ("    -remotelist         : List new remote server");
      Logs.Write ("    -remoteadd          : Add a new remote server");
   end Usage;

begin
   GNAT.Command_Line.Initialize_Option_Scan (Section_Delimiters => "remote");

   Interate_On_Opt : loop
      case GNAT.Command_Line.Getopt
           ("V verbose VV very_verbose L: version v "
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

         when 'L' =>
            Complete_L : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "L" then
                  Logs.Set_File (GNAT.Command_Line.Parameter);
               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_L;

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
