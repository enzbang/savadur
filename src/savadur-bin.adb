------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2008                             --
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
--  savadur [GENERAL_OPTIONS] --client|--server|--standalone [MODE_OPTIONS]

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Savadur.Client_Service.Client;
with Savadur.Client_Service.Types;
with Savadur.Config.Client;
with Savadur.Config.Environment_Variables;
with Savadur.Config.Project;
with Savadur.Config.Project_List;
with Savadur.Config.Notifications.SMTP;
with Savadur.Config.Notifications.XMPP;
with Savadur.Config.SCM;
with Savadur.Config.Server;
with Savadur.Jobs.Client;
with Savadur.Logs;
with Savadur.Projects;
with Savadur.Utils;
with Savadur.Version;
with Savadur.SCM;
with Savadur.Scenarios;
with Savadur.Servers;
with Savadur.Signed_Files;
with Savadur.Web.Client;
with Savadur.Web.Server;

with SOAP;

with GNAT.Command_Line;

procedure Savadur.Bin is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   type Running_Mode is (Savadur_Client, Savadur_Server, Savadur_Standalone);
   Mode : Running_Mode;

   Syntax_Error    : exception;

   Action : access procedure;
   --  Action to execute after Command Line parsing

   --  client.xml configuration
   Client_Id       : Unbounded_String;
   Client_Endpoint : Unbounded_String;

   --  Savadur-Client remote server
   New_Server_Name : Unbounded_String;
   New_Server_URL  : Unbounded_String;

   --   Standalone mode
   Project_Name    : Unbounded_String;
   Scenario_Id     : Unbounded_String
     := Scenarios.Id_Utils.To_Unbounded_String (Scenarios.Default_Scenario);

   procedure Add_Remote_Server;
   --  Add a new remote server

   procedure List_Remote_Server;
   --  Lists remote servers

   procedure Run_Client_Mode;
   --  Run client mode

   procedure Run_Server_Mode;
   --  Run server mode

   procedure Run_Standalone;
   --  Run standalone

   procedure Set_Client_Config;
   --  Sets client config (id and endpoint)

   procedure Usage (Error_Message : in String := "");
   --  Displays Usage

   -----------------------
   -- Add_Remote_Server --
   -----------------------

   procedure Add_Remote_Server is
      use Ada.Text_IO;
      Filename : constant String :=
                   Directories.Compose
                     (Containing_Directory => Config.Server_Directory,
                      Name                 => -New_Server_Name,
                      Extension            => "xml");
      File     : File_Type;
   begin
      if New_Server_Name = Null_Unbounded_String
        or else New_Server_URL = Null_Unbounded_String
      then
         Usage
           (Error_Message => "Can't add new remote server. Wrong arguments");
         return;
      end if;

      Create (File => File, Mode => Out_File, Name => Filename);

      Put_Line ("Add new remote server : "
                & (-New_Server_Name) & " " & (-New_Server_URL));

      Put_Line (File, "<server>");
      Put_Line (File, "<name value='" & (-New_Server_Name) & "'/>");
      Put_Line (File, "<location url='" & (-New_Server_URL) & "'/>");
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

      if Servers.Length = 0 then
         Logs.Write
           (Content => "No server configured",
            Kind    => Logs.Handler.Error);

      else
         Logs.Write (Savadur.Servers.Image);
      end if;
   end List_Remote_Server;

   ---------------------
   -- Run_Client_Mode --
   ---------------------

   procedure Run_Client_Mode is

      use type Containers.Count_Type;

      task Keep_Alive;
      --  Schedule ping and register calls

      procedure Ping (Cursor : in Servers.Cursor);
      --  Ping a server

      procedure Register (Cursor : in Servers.Cursor);
      --  Registers client to the pointer server

      ----------------
      -- Keep_Alive --
      ----------------

      task body Keep_Alive is
         use type Calendar.Time;

         Ping_Delay  : constant Duration := Config.Client.Get_Ping_Delay;
         Retry_Delay : constant Duration :=
                         Config.Client.Get_Connection_Retry_Delay;

         type To_Run is (Connect, Ping);
         Run : To_Run := Ping;

         Next_Ping    : Calendar.Time := Calendar.Clock + Ping_Delay;
         Next_Connect : Calendar.Time := Calendar.Clock + Retry_Delay;
         Next_Time    : Calendar.Time;
      begin

         Keep_Alive_Loop : loop

            if Next_Ping < Next_Connect then
               Next_Time := Next_Ping;
               Run := Ping;
            else
               Next_Time := Next_Connect;
               Run := Connect;
            end if;

            delay until Next_Time;

            --  Run and reschedule

            if Run = Connect then

               --  Try to connect offline servers

               Savadur.Servers.Offline_Iterate (Register'Access);
               Next_Connect := Calendar.Clock + Retry_Delay;
            else

               --  Try to ping online servers

               Savadur.Servers.Online_Iterate (Ping'Access);
               Next_Ping := Calendar.Clock + Ping_Delay;
            end if;
         end loop Keep_Alive_Loop;
      end Keep_Alive;

      ----------
      -- Ping --
      ----------

      procedure Ping (Cursor : in Servers.Cursor) is
         Server_Name : constant String := Servers.Name (Cursor);
         Server_URL  : constant String := Servers.URL (Cursor);
      begin
         Logs.Write
           (Content =>
              "Ping " & Server_Name & " at " & Server_URL,
            Kind    => Logs.Handler.Very_Verbose);

         Logs.Write
           (Content => "Receive " & Client_Service.Client.Ping
              (Endpoint => Server_URL),
            Kind    => Logs.Handler.Very_Verbose);

      exception
         when SOAP.SOAP_Error =>
            Logs.Write (Content => "Ping " & Server_Name & " failed !",
                        Kind    => Logs.Handler.Warnings);
            Savadur.Servers.Go_Offline (+Server_Name);
      end Ping;

      --------------
      -- Register --
      --------------

      procedure Register (Cursor : in Servers.Cursor) is
         Server_Name : constant String := Servers.Name (Cursor);
         Server_URL  : constant String := Servers.URL (Cursor);
         Metadata    : constant Client_Service.Types.Metadata_Type :=
                         Savadur.Config.Client.Get_Metadata;
         Key         : constant String := Config.Client.Get_Key;
         Endpoint    : constant String := Config.Client.Get_Endpoint;
      begin
         Logs.Write
           (Content =>
              "Register to " & Server_Name & " at " & Server_URL,
            Kind    => Logs.Handler.Information);

         Client_Service.Client.Register
           (Key               => Key,
            Data              => Metadata,
            Server_Name       => Server_Name,
            Callback_Endpoint => Endpoint,
            Endpoint          => Server_URL);

         Logs.Write (Content => "Done.",
                     Kind    => Logs.Handler.Information);

         Savadur.Servers.Go_Online (+Server_Name);
      exception
         when SOAP.SOAP_Error =>
            Logs.Write (Content => "Register to " & Server_Name
                        & " failed !");
            Savadur.Servers.Go_Offline (+Server_Name);
      end Register;
   begin
      --  Parse configuration files

      Config.SCM.Parse;
      Config.Server.Parse;
      Config.Project.Parse;
      Config.Client_Server := True;

      Logs.Write
        (Content => "SCM Found" & ASCII.LF
         & Savadur.SCM.Image (Savadur.Config.SCM.Configurations)
         & ASCII.LF,
         Kind    => Logs.Handler.Very_Verbose);

      if Servers.Length = 0 then
         Logs.Write
           (Content => "No server configured",
            Kind    => Logs.Handler.Error);

      else
         --  Start the server

         Web.Client.Start;

         --  Register this client to all known server

         Savadur.Servers.Offline_Iterate (Register'Access);

         --  Loop every 5 minutes trying to reconnect
         --  Ping servers every 10 minutes to check if all is working

      end if;
   end Run_Client_Mode;

   ---------------------
   -- Run_Server_Mode --
   ---------------------

   procedure Run_Server_Mode is
   begin
      Config.Is_Server := True;

      Config.SCM.Parse;
      Config.Project.Parse;
      Config.Project_List.Parse;
      Config.Notifications.XMPP.Parse;
      Config.Notifications.SMTP.Parse;

      Web.Server.Start;

      --  Endless loop

      loop
         delay Duration'Last;
      end loop;
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
         Kind    => Logs.Handler.Very_Verbose);

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
            Kind    => Logs.Handler.Verbose);

         Signed_Files.Create
           (Signed_Project,
            -Project_Name,
            Projects.Project_Filename (Project'Access));

         Jobs.Client.Queue.Add (Signed_Project, "", -Scenario_Id);
      end Run_Project;

      Jobs.Client.Queue.Stop;
   end Run_Standalone;

   -------------------------
   --  Set_Client_Config  --
   -------------------------

   procedure Set_Client_Config is
      use Ada.Text_IO;
      File                   : File_Type;
      Config_Client_Id       : constant String :=
                                 Savadur.Config.Client.Get_Key;
      Config_Client_Endpoint : constant String :=
                                 Savadur.Config.Client.Get_Endpoint;
      Filename               : constant String := Directories.Compose
        (Containing_Directory => Config.Savadur_Directory,
         Name                 => "client",
         Extension            => "xml");
   begin
      if not Directories.Exists (Config.Savadur_Directory) then
         Directories.Create_Directory (Config.Savadur_Directory);
      end if;

      Create (File => File, Mode => Out_File, Name => Filename);
      Put_Line (File, "<client>");

      if -Client_Id /= "" then
         Put_Line (File, "<name id='" & (-Client_Id) & "'/>");
      elsif Config_Client_Id /= "" then
         Put_Line (File, "<name id='" & Config_Client_Id & "'/>");
      end if;

      if -Client_Endpoint /= "" then
         Put_Line (File, "<endpoint url='" & (-Client_Endpoint) & "'/>");
      elsif Config_Client_Endpoint /= "" then
         Put_Line (File, "<endpoint url='" & Config_Client_Endpoint & "'/>");
      end if;

      Put_Line (File, "</client>");
      Close (File);
   end Set_Client_Config;

   -----------
   -- Usage --
   -----------

   procedure Usage (Error_Message : in String := "") is
      procedure Endpoint_Config;
      --  Prints Usage for configuring endpoint and id

      procedure Global_Options;
      --  Prints global options

      ---------------------
      -- Endpoint_Config --
      ---------------------

      procedure Endpoint_Config is
      begin
         Logs.Write ("    --config --endpoint endpoint :"
                     & " Set client endpoint");
         Logs.Write ("    --config --id  client_id : Set client id");
      end Endpoint_Config;

      --------------------
      -- Global_Options --
      --------------------

      procedure Global_Options is
      begin
         Logs.Write ("OPTIONS :");
         Logs.Write ("    --savadurdir dirname : set savadur directory");
         Logs.Write ("        ($SAVADUR_DIR or $HOME/.savadur by default)");
         Logs.Write ("    -v  | --version");
         Logs.Write ("    -V  | --verbose");
         Logs.Write ("    -VV | --very-verbose");
         Logs.Write ("    -L filename          : filename for log file");
      end Global_Options;
   begin
      --  Display error message if not null

      if Error_Message /= "" then
         Logs.Write (Content => Error_Message, Kind => Logs.Handler.Error);

         --  Set exit status to Failure
         Command_Line.Set_Exit_Status (Command_Line.Failure);
      end if;

      Logs.Write ("Savadur " & Version.Simple);
      case Mode is
         when Savadur_Client =>
            Logs.Write ("usage : savadur --client [OPTIONS]");
            Global_Options;
            Endpoint_Config;
            Logs.Write ("    --remote --list      : List new remote server");
            Logs.Write ("    --remote --add server_name server_url"
                        & " : Add a new remote server");

         when Savadur_Standalone =>
            Logs.Write ("usage: savadur --standalone [OPTIONS]");
            Global_Options;
            Logs.Write ("    --project PROJECT NAME --scenario SCENARIO");

         when Savadur_Server =>
            Global_Options;
            Endpoint_Config;
            null;
      end case;
   end Usage;

begin
   GNAT.Command_Line.Initialize_Option_Scan
     (Section_Delimiters => "-remote -config -project");

   --  Get running mode

   Get_Running_Mode : declare
      procedure Mode_Usage;
      --  Display mode usage

      procedure Mode_Usage is
      begin
         Text_IO.Put_Line
           ("Usage: savadur --server|--client|--standalone OPTIONS");
         Text_IO.Put_Line
           ("Run savadur --MODE --help for specific MODE usage");
         Command_Line.Set_Exit_Status (Command_Line.Failure);
      end Mode_Usage;

   begin
      case GNAT.Command_Line.Getopt ("-server -client -standalone") is
      when '-' =>
         Long_Running_Mode : declare
            Full : constant String := GNAT.Command_Line.Full_Switch;
         begin
            if Full = "-server" then
               Mode := Savadur_Server;
            elsif Full = "-client" then
               Mode := Savadur_Client;
            elsif Full = "-standalone" then
               Mode := Savadur_Standalone;
            end if;
         end Long_Running_Mode;

      when others =>
         Mode_Usage;
         return;
      end case;

   exception
      when GNAT.Command_Line.Invalid_Section
         | GNAT.Command_Line.Invalid_Switch
         | GNAT.Command_Line.Invalid_Parameter =>
         Mode_Usage;
         return;
   end Get_Running_Mode;

   Main_Section : loop
      case GNAT.Command_Line.Getopt
        ("V -verbose VV -very-verbose L: -version v "
         & "-help -savadurdir:")
      is
         when ASCII.NUL =>
            exit Main_Section;

         when '-' =>
            --  Handle all log commands
            Long_Options : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "-version" then
                  Text_IO.Put_Line ("Savadur " & Savadur.Version.Complete);
               elsif Full = "-help" then
                  Usage;
                  return;
               elsif Full = "-savadurdir" then
                  Config.Set_Savadur_Directory (GNAT.Command_Line.Parameter);
               elsif Full = "-verbose" then
                  Logs.Handler.Set
                    (Kind      => Logs.Handler.Verbose,
                     Activated => True);

               elsif Full = "-very-verbose" then
                  Logs.Handler.Set
                    (Kind      => Logs.Handler.Verbose,
                     Activated => True);
                  Logs.Handler.Set
                    (Kind      => Logs.Handler.Very_Verbose,
                     Activated => True);
               end if;
            end Long_Options;

         when 'v' | 'V' =>
            Complete_V : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "V"  then
                  Logs.Handler.Set
                    (Kind      => Logs.Handler.Verbose,
                     Activated => True);

               elsif Full = "VV" then
                  Logs.Handler.Set
                    (Kind      => Logs.Handler.Verbose,
                     Activated => True);
                  Logs.Handler.Set
                    (Kind      => Logs.Handler.Very_Verbose,
                     Activated => True);

               elsif Full = "v" then
                  Logs.Write
                    (Content => "Savadur " & Savadur.Version.Complete);
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
                  Logs.Handler.Set_File (GNAT.Command_Line.Parameter);
               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_L;

         when others =>
            Usage (Error_Message => "Unknown syntax");
            return;
      end case;
   end loop Main_Section;

   GNAT.Command_Line.Goto_Section (Name => "-remote");

   if Mode = Savadur_Client then
      Remote_Opt : loop
         case GNAT.Command_Line.Getopt ("* -add -list") is
            when ASCII.NUL =>
               exit Remote_Opt;

            when '-' =>
               Long_Options_Remote : declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "-add" then
                     Action := Add_Remote_Server'Access;

                  elsif Full = "-list" then
                     Action := List_Remote_Server'Access;

                  else
                     raise Syntax_Error with "Unknown option " & Full;
                  end if;
               end Long_Options_Remote;

            when '*' =>
               if New_Server_Name = Null_Unbounded_String then
                  New_Server_Name := +GNAT.Command_Line.Full_Switch;
               else
                  New_Server_URL  := +GNAT.Command_Line.Full_Switch;
               end if;

            when others =>
               Usage (Error_Message => "(remote) unknown syntax");
               return;
         end case;
      end loop Remote_Opt;
   end if;

   GNAT.Command_Line.Goto_Section (Name => "-project");

   if Mode = Savadur_Standalone then
      Project_Opt : loop
         case GNAT.Command_Line.Getopt ("* -scenario:") is
            when ASCII.Nul =>
               exit Project_Opt;

            when '*' =>
               if Project_Name = +"" then
                  --  Set project name (take first parameter only)

                  Project_Name :=
                    To_Unbounded_String (GNAT.Command_Line.Full_Switch);
               else
                  Usage (Error_Message => "(project) unknown syntax");
                  return;
               end if;
               Action := Run_Standalone'Access;

            when '-' =>
               Long_Options_Standalone : declare
                  Full : constant String := GNAT.Command_Line.Full_Switch;
               begin
                  if Full = "-scenario" then
                     Scenario_Id :=
                       To_Unbounded_String (GNAT.Command_Line.Parameter);
                  end if;
               end Long_Options_Standalone;

            when others =>
               Usage (Error_Message => "(project) unknown syntax");
               return;
         end case;
      end loop Project_Opt;
   end if;

   --  Config section

   GNAT.Command_Line.Goto_Section (Name => "-config");

   Config_Opt : loop
      case GNAT.Command_Line.Getopt ("-id: -endpoint:") is
         when ASCII.NUL =>
            exit Config_Opt;

         when '-' =>
            Long_Options_Config : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "-endpoint" then
                  Action := Set_Client_Config'Access;
                  Client_Endpoint :=
                    To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif Full = "-id" then
                  Action := Set_Client_Config'Access;
                  Client_Id :=
                    To_Unbounded_String (GNAT.Command_Line.Parameter);

               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Long_Options_Config;

         when others =>
            Usage (Error_Message => "(config) unknown syntax");
            return;
      end case;
   end loop Config_Opt;

   if Action /= null then
      Action.all;
   else
      case Mode is
         when Savadur_Client =>
            Run_Client_Mode;

         when Savadur_Server =>
            Run_Server_Mode;

         when Savadur_Standalone =>
            null;
      end case;
      Usage;
   end if;

exception
   when GNAT.Command_Line.Invalid_Section
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter
        =>
      Usage (Error_Message => "INVALID : unknown syntax");

   when E : Savadur.Config.Config_Error
      | Savadur.Config.Project.Config_Error
      | Savadur.Config.Environment_Variables.Config_Error
        =>
      Usage (Error_Message => Exceptions.Exception_Message (E));

   when others =>
      Usage (Error_Message =>
             "Unknown error - should have been caught before !!!");
end Savadur.Bin;
