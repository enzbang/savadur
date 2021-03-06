------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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

with IO_Exceptions;

with Ada.Directories;
with Ada.Strings.Unbounded;

with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Server;
with AWS.Status;
with AWS.Templates;
with AWS.URL;

with SOAP.Dispatchers.Callback;

with Savadur.Client_Service.CB;
with Savadur.Clients;
with Savadur.Config.Client;
with Savadur.Config.Project;
with Savadur.Config.Project_List;
with Savadur.Database;
with Savadur.Jobs.Server;
with Savadur.Notifications;
with Savadur.Projects;
with Savadur.Project_List;
with Savadur.Remote_Files;
with Savadur.Scenarios;
with Savadur.Server_Service.Client;
with Savadur.Signed_Files;
with Savadur.Utils;
with Savadur.Logs;

package body Savadur.Web.Server is

   use Ada;
   use AWS;
   use SOAP;

   HTTP       : AWS.Server.HTTP;
   Config     : AWS.Config.Object := AWS.Config.Get_Current;
   Dispatcher : Client_Service.CB.Handler;
   Address    : URL.Object;

   function HTTP_Callback (Request : in Status.Data) return Response.Data;
   --  Callbacks used for HTTP requests

   function Run (Request : in Status.Data) return Response.Data;
   --  Runs a project

   function List (Request : in Status.Data) return Response.Data;
   --  Displays the project list

   procedure Ping
     (Key : in String; Server_Name : in String; Endpoint : in String);
   --  Ping a client

   function Ping return Response.Data;
   --  Pings clients

   function Show_Log (Log_Id : in String) return Response.Data;
   --  Show the selected log content

   function Show_Project (Request : in Status.Data) return Response.Data;
   --  Displays the project status

   function Notifications (Request : in Status.Data) return Response.Data;
   --  The notifications page

   -------------------
   -- HTTP_Callback --
   -------------------

   function HTTP_Callback (Request : in Status.Data) return Response.Data is
      URI       : constant String := Status.URI (Request);
      Root_URI  : constant String := "/";
      Run_URI   : constant String := "/run";
      Ping_URI  : constant String := "/ping";
      RSS_URI   : constant String := "/rss/all";
      Log_URI   : constant String := "/log/";
      CSS_URI   : constant String := "/css/";
      Img_URI   : constant String := "/img/";
      Notif_URI : constant String := "/notif";
   begin
      Logs.Write
        (Content => "Calling => " & URI,
         Kind    => Logs.Handler.Very_Verbose);

      if URI = Root_URI then
         return List (Request);

      elsif URI = Run_URI then
         return Run (Request);

      elsif URI = Notif_URI then
         return Notifications (Request);

      elsif URI = Ping_URI then
         return Ping;

      elsif URI = RSS_URI then
         Get_RSS : declare
            RSS_File : constant String :=
              (Directories.Compose
                 (Containing_Directory => Savadur.Config.RSS_Directory,
                  Name                 => "all",
                  Extension            => "xml"));
         begin
            if Directories.Exists (RSS_File) then
               return Response.File (MIME.Text_XML, RSS_File);
            end if;

            --  Try to generate RSS

            Savadur.Notifications.Update_RSS;

            if Directories.Exists (RSS_File) then
               return Response.File (MIME.Text_XML, RSS_File);
            else
               return Response.Build
                 (Content_Type => MIME.Text_HTML,
                  Message_Body => "<p>File " & RSS_File & " not found</p>",
                  Status_Code  => Messages.S404);
            end if;
         end Get_RSS;

      elsif URI'Length > Log_URI'Length
        and then URI (URI'First .. URI'First + Log_URI'Length - 1) = Log_URI
      then
         return Show_Log (URI (URI'First + Log_URI'Length .. URI'Last));

      elsif URI'Length > CSS_URI'Length
        and then URI (URI'First .. URI'First + CSS_URI'Length - 1) = CSS_URI
      then
         Get_CSS : declare
            CSS_File : constant String := Directories.Compose
              (Containing_Directory => Savadur.Config.Web_CSS_Directory,
               Name                 => URI
                 (URI'First + CSS_URI'Length .. URI'Last));
         begin
            if Directories.Exists (CSS_File) then
               Logs.Write (CSS_File);
               return Response.File (MIME.Text_CSS, CSS_File);
            else
               return Response.Build
                 (MIME.Text_HTML,
                  "<p>File " & CSS_File & " not found</p>",
                  Status_Code => Messages.S404);
            end if;
         end Get_CSS;

      elsif URI'Length > Img_URI'Length
        and then URI (URI'First .. URI'First + Img_URI'Length - 1) = Img_URI
      then
         Get_Img : declare
            Img_File : constant String := Directories.Compose
              (Containing_Directory => Savadur.Config.Web_Img_Directory,
               Name                 => URI
                 (URI'First + Img_URI'Length .. URI'Last));
         begin
            if Directories.Exists (Img_File) then
               Logs.Write (Img_File);
               return Response.File (MIME.Content_Type (Img_File), Img_File);
            else
               return Response.Build
                 (MIME.Text_HTML,
                  "<p>File " & Img_File & " not found</p>",
                  Status_Code => Messages.S404);
            end if;
         end Get_Img;

      elsif Savadur.Config.Project.Is_Project_Name
        (URI (URI'First + 1 .. URI'Last))
      then
         return Show_Project (Request);
      end if;

      return Response.Build
        (MIME.Text_HTML, "<p>" & URI & " not found</p>",
         Status_Code => Messages.S404);

   exception
      when others =>
         return Response.Build
           (MIME.Text_HTML,
            "<p>" & URI & " error returned</p>",
            Status_Code => Messages.S200);
   end HTTP_Callback;

   ----------
   -- List --
   ----------

   function List (Request : in Status.Data) return Response.Data is
      pragma Unreferenced (Request);
      use type Templates.Tag;
      Set : Templates.Translate_Set :=
              Project_List.To_Set
                (Savadur.Config.Project_List.Configurations.all);
   begin
      Templates.Insert (Set, Clients.Clients_Set);
      Templates.Insert
        (Set, Templates.Assoc ("SERVER_NAME", Savadur.Config.Client.Get_Key));

      return AWS.Response.Build
        (Content_Type => MIME.Text_HTML,
         Message_Body => AWS.Templates.Parse
           (Filename     => Directories.Compose
              (Containing_Directory => Savadur.Config.Web_Templates_Directory,
               Name                 => "list",
               Extension            => "thtml"),
            Translations => Set));
   end List;

   -------------------
   -- Notifications --
   -------------------

   function Notifications (Request : in Status.Data) return Response.Data is
      P            : constant AWS.Parameters.List :=
                       Status.Parameters (Request);
      Action       : constant String := Parameters.Get (P, "a");
      Project_Name : constant String := Parameters.Get (P, "p");
      Set          : Templates.Translate_Set;
   begin
      if Action = "add" then
         Database.Add_Notification
           (Project_Name,
            Parameters.Get (P, "email"),
            Parameters.Get (P, "log_email"),
            Parameters.Get (P, "xmpp"),
            Parameters.Get (P, "log_xmpp"));

      elsif Action = "del" then
         declare
            use Ada.Strings.Unbounded;
            Ids : constant Parameters.VString_Array :=
                    Parameters.Get_Values (P, "delid");
            Sep : Natural;
         begin
            for K in Ids'Range loop
               Sep := Index (Ids (K), "/");

               if Sep /= 0 then
                  Database.Del_Notification
                    (Project_Name,
                     Slice (Ids (K), 1, Sep - 1),
                     Slice (Ids (K), Sep + 1, Length (Ids (K))));
               end if;
            end loop;
         end;
      end if;

      Set := Database.Get_Notifications (Project_Name);

      Templates.Insert (Set, Templates.Assoc ("PROJECT_NAME", Project_Name));

      return AWS.Response.Build
        (Content_Type => MIME.Text_HTML,
         Message_Body => AWS.Templates.Parse
           (Filename     => Directories.Compose
              (Containing_Directory =>
                 Savadur.Config.Web_Templates_Directory,
               Name                 => "notifications",
               Extension            => "thtml"),
            Translations => Set));
   end Notifications;

   ----------
   -- Ping --
   ----------

   procedure Ping
     (Key : in String; Server_Name : in String; Endpoint : in String)
   is
      pragma Unreferenced (Server_Name);
   begin
      Logs.Write ("Ping client " & Key);
      Logs.Write
        ("Get " & Server_Service.Client.Ping (Endpoint => Endpoint));

   exception
      when SOAP.SOAP_Error =>
         --  Client if offline.
         --  Removes it from online clients and add logout info
         --  into database

         Database.Logout (Key);
         Clients.Set_Status (Key, Clients.Offline);
   end Ping;

   ----------
   -- Ping --
   ----------

   function Ping return Response.Data is
      use Ada.Strings.Unbounded;
      use Savadur.Utils;
   begin
      Clients.Iterate (Clients.Idle, Ping'Access);

      return AWS.Response.File
        (Content_Type => MIME.Text_HTML,
         Filename     => Directories.Compose
           (Containing_Directory =>
              Savadur.Config.Web_Templates_Directory,
            Name                 => "ping",
            Extension            => "html"));
   end Ping;

   ---------
   -- Run --
   ---------

   function Run (Request : in Status.Data) return Response.Data is
      P            : constant AWS.Parameters.List :=
                       Status.Parameters (Request);
      Project_Name : constant String := Parameters.Get (P, "p");
      Scenario_Id  : constant String := Parameters.Get (P, "s");
      Latency      : constant String := Parameters.Get (P, "l");

      function Patch_Filename return String;
      --  Returns the patch filename or "" if none

      function Response
        (Set : in Templates.Translate_Set) return AWS.Response.Data;

      --------------------
      -- Patch_Filename --
      --------------------

      function Patch_Filename return String is
         Filename : constant String := Parameters.Get (P, "filename");
      begin
         if Filename /= "" then
            Save_File : declare
               Simple_Name    : constant String :=
                                  Directories.Simple_Name (Filename);
               Patch_Filename : constant String := Utils.Unique_Filename
                 (Directories.Compose
                    (Containing_Directory => Savadur.Config.Patch_Directory,
                     Name                 => Simple_Name));
            begin
               Logs.Write ("Get new patch " & Patch_Filename);
               Directories.Rename (Filename, Patch_Filename);

               return Directories.Simple_Name (Patch_Filename);
            end Save_File;
         end if;

         return "";
      end Patch_Filename;

      --------------
      -- Response --
      --------------

      function Response
        (Set : in Templates.Translate_Set) return AWS.Response.Data is
      begin
         return AWS.Response.Build
           (Content_Type => MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse
              (Filename     => Directories.Compose
                 (Containing_Directory =>
                    Savadur.Config.Web_Templates_Directory,
                  Name                 => "run",
                  Extension            => "thtml"),
               Translations => Set));
      end Response;

      Set : Templates.Translate_Set;

   begin
      if Project_Name = "" or else Scenario_Id = "" then
         Templates.Insert
           (Set,
            Templates.Assoc
              ("ERROR_MESSAGE",
               "A project and a scenario must be specified : "
               & " http://server:port/run?p=project&s=scenario"));
         return Response (Set);
      end if;

      --  Check if scenario exists for this project

      Check_Scenario : declare
         use Savadur.Utils;
         Configuration : constant Projects.Project_Config :=
                           Savadur.Remote_Files.Load_Project (Project_Name);
      begin
         if not Scenarios.Has_Scenario
           (Configuration.Scenarios, Scenario_Id)
         then
            Templates.Insert
              (Set,
               Templates.Assoc
                 ("ERROR_MESSAGE",
                  "The given scenario (" & Scenario_Id & ") does not exists"));
            return Response (Set);
         end if;
      end Check_Scenario;

      Run_Project : declare
         Project          : aliased Projects.Project_Config :=
                              Savadur.Config.Project.Get (Project_Name);
         Project_Filename : constant String :=
                              Projects.Project_Filename (Project'Access);
         Signed_Project   : Signed_Files.Handler;
      begin
         Signed_Files.Create (Signed_Project, Project_Name, Project_Filename);

         if Latency = "" then
            Jobs.Server.Queue.Add
              (Project  => Signed_Project,
               Patch    => Patch_Filename,
               Server   => "",
               Scenario => Scenario_Id);
         else
            Jobs.Server.Queue.Add
              (Project  => Signed_Project,
               Patch    => Patch_Filename,
               Server   => "",
               Scenario => Scenario_Id,
               Latency  => Duration'Value (Latency));
         end if;

         Templates.Insert
           (Set, Templates.Assoc ("PROJECT_NAME", Project_Name));
         Templates.Insert
           (Set, Templates.Assoc ("SCENARIO", Scenario_Id));

         return Response (Set);
      end Run_Project;
   exception
      when IO_Exceptions.Name_Error =>
         Templates.Insert
           (Set,
            Templates.Assoc
              ("ERROR_MESSAGE",
               "Project " & Project_Name & " Not found."));
         return Response (Set);
   end Run;

   ----------------
   --  Show_Log  --
   ----------------

   function Show_Log (Log_Id : in String) return Response.Data is
   begin
      Get_Content : declare
         Id : constant Positive := Positive'Value (Log_Id);
      begin
         return AWS.Response.Build
           (Content_Type => MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse
              (Filename     => Directories.Compose
                 (Containing_Directory =>
                    Savadur.Config.Web_Templates_Directory,
                  Name                 => "log",
                  Extension            => "thtml"),
               Translations => Database.Get_Log_Content (Id)));
      end Get_Content;
   exception
      when Constraint_Error =>
         return Response.Build
           (MIME.Text_HTML, "<p>Wrong request</p>",
            Status_Code => Messages.S200);
   end Show_Log;

   --------------------
   --  Show_Project  --
   --------------------

   function Show_Project (Request : in Status.Data) return Response.Data is

      procedure Process_Scenario (Position : in Savadur.Scenarios.Sets.Cursor);
      --  Add given scenario into the corresponding vector tag

      URI           : constant String := Status.URI (Request);
      Project_Name  : constant String := URI (URI'First + 1 .. URI'Last);
      Configuration : constant Projects.Project_Config :=
                        Savadur.Remote_Files.Load_Project (Project_Name);
      Set           : Templates.Translate_Set :=
                        Savadur.Database.Get_Final_Status
                          (Project_Name => Project_Name);
      Scenarios     : Templates.Tag;

      ----------------------
      -- Process_Scenario --
      ----------------------

      procedure Process_Scenario
        (Position : in Savadur.Scenarios.Sets.Cursor)
      is
         Scenario : constant Savadur.Scenarios.Scenario :=
                      Savadur.Scenarios.Sets.Element (Position);
      begin
         Templates.Append
           (Scenarios, Savadur.Scenarios.Id_Utils.To_String (Scenario.Id));
      end Process_Scenario;

   begin
      Configuration.Scenarios.Iterate (Process_Scenario'Access);

      Templates.Insert
        (Set, Templates.Assoc ("PROJECT_NAME", Project_Name));
      Templates.Insert
        (Set, Templates.Assoc
           ("PROJECT_DESCRIPTION",
            Projects.Desc_Utils.To_String (Configuration.Description)));
      Templates.Insert
        (Set, Templates.Assoc ("SCENARIOS", Scenarios));

      Templates.Insert
        (Set, Savadur.Database.Get_Logs
           (Project_Name, Project_List.Get_Log_Size (Project_Name)));

      return AWS.Response.Build
        (Content_Type => MIME.Text_HTML,
         Message_Body => AWS.Templates.Parse
           (Filename     => Directories.Compose
              (Containing_Directory => Savadur.Config.Web_Templates_Directory,
               Name                 => "project_page",
               Extension            => "thtml"),
            Translations => Set));
   end Show_Project;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Address := URL.Parse (Savadur.Config.Client.Get_Endpoint);
      AWS.Config.Set.Server_Port (Config, URL.Port (Address));

      Dispatcher := Dispatchers.Callback.Create
        (HTTP_Callback'Access, Client_Service.CB.SOAP_CB'Access);

      AWS.Server.Start (HTTP, Dispatcher, Config);

      Jobs.Server.Queue.Add_Periodic_Scenario;

      Logs.Write
        (Content => "Server started on port " & URL.Port (Address),
         Kind    => Logs.Handler.Information);
   end Start;

end Savadur.Web.Server;
