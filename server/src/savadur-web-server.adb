------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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
with Savadur.Config.Project;
with Savadur.Config.Project_List;
with Savadur.Database;
with Savadur.Jobs.Server;
with Savadur.Projects;
with Savadur.Project_List;
with Savadur.Remote_Files;
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

   function Ping return Response.Data;
   --  Pings clients

   function Show_Log (Log_Id : in String) return Response.Data;
   --  Show the selected log content

   function Show_Project (Request : in Status.Data) return Response.Data;
   --  Displays the project status

   -------------------
   -- HTTP_Callback --
   -------------------

   function HTTP_Callback (Request : in Status.Data) return Response.Data is
      Web_Dir : constant String :=
                  Directories.Compose
                    (Containing_Directory => Savadur.Config.Savadur_Directory,
                     Name                 => "htdocs");
      Web_CSS : constant String :=
                  Directories.Compose
                    (Containing_Directory => Web_Dir,
                     Name                 => "css");
      URI     : constant String := Status.URI (Request);
      Log_URI : constant String := "/log/";
      CSS_URI : constant String := "/css";
   begin
      Logs.Write
        (Content => "Calling => " & URI,
         Kind    => Logs.Handler.Very_Verbose);

      if URI = "/" then
         return List (Request);

      elsif URI = "/run" then
         return Run (Request);

      elsif URI = "/ping" then
         return Ping;

      elsif URI'Length > Log_URI'Length
        and then URI (URI'First .. URI'First + Log_URI'Length - 1) = Log_URI
      then
         return Show_Log (URI (URI'First + Log_URI'Length .. URI'Last));

      elsif URI'Length > CSS_URI'Length
        and then URI (URI'First .. URI'First + CSS_URI'Length - 1) = CSS_URI
      then
         Get_CSS : declare
            CSS_File : constant String := Directories.Compose
              (Containing_Directory => Web_CSS,
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
      Web_Dir       : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "htdocs");
      Web_Templates : constant String := Directories.Compose
        (Containing_Directory => Web_Dir,
         Name                 => "templates");
   begin
      return AWS.Response.Build
        (Content_Type => MIME.Text_HTML,
         Message_Body => AWS.Templates.Parse
           (Filename     => Directories.Compose
              (Containing_Directory => Web_Templates,
               Name                 => "list",
               Extension            => "thtml"),
            Translations => Project_List.To_Set
              (Savadur.Config.Project_List.Configurations)));
   end List;

   ----------
   -- Ping --
   ----------

   function Ping return Response.Data is
      use Ada.Strings.Unbounded;
      use Savadur.Utils;
      P_Client        : Clients.Sets.Cursor := Clients.Registered.First;
      Offline_Clients : Unbounded_String;
      Online_Clients  : Unbounded_String;
   begin
      while Clients.Sets.Has_Element (P_Client) loop
         Ping_Client : declare
            Client : constant Clients.Client :=
                       Clients.Sets.Element (P_Client);
         begin
            Clients.Sets.Next (P_Client);

            Logs.Write ("Ping client " & (-Client.Key));
            Logs.Write
              ("Get " &
               Server_Service.Client.Ping
                 (Endpoint => -Client.Callback_Endpoint));
            Append (Online_Clients, Client.Key);
         exception
            when SOAP.SOAP_Error =>
               --  Client if offline.
               --  Removes it from online clients and add logout info
               --  into database

               Database.Logout (-Client.Key);
               Clients.Sets.Delete (Clients.Registered, Client);
               Append (Offline_Clients, Client.Key);
         end Ping_Client;
      end loop;

         return Response.Build
           (MIME.Text_HTML,
            "<p>Online clients : " & Online_Clients & "...</p>"
            & "<p>Offline clients : " & Offline_Clients & "...</p>");
   end Ping;

   ---------
   -- Run --
   ---------

   function Run (Request : in Status.Data) return Response.Data is
      P            : constant AWS.Parameters.List :=
                       Status.Parameters (Request);
      Project_Name : constant String := Parameters.Get (P, "p");
      Scenario_Id  : constant String := Parameters.Get (P, "s");
   begin
      Run_Project : declare
         Project          : aliased Projects.Project_Config :=
                              Savadur.Config.Project.Get (Project_Name);
         Project_Filename : constant String :=
                              Projects.Project_Filename (Project'Access);
         Signed_Project   : Signed_Files.Handler;
      begin
         Signed_Files.Create (Signed_Project, Project_Name, Project_Filename);

         Jobs.Server.Queue.Add (Signed_Project, "", Scenario_Id);

         return Response.Build
           (MIME.Text_HTML,
            "<p>Running " & Project_Name & "...</p>");
      end Run_Project;
   exception
      when IO_Exceptions.Name_Error =>
         return Response.Build
           (MIME.Text_HTML,
            "<p>Project " & Project_Name & " Not found</p>",
            Status_Code => Messages.S200);
   end Run;

   ----------------
   --  Show_Log  --
   ----------------

   function Show_Log (Log_Id : in String) return Response.Data is
      Web_Dir       : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "htdocs");
      Web_Templates : constant String := Directories.Compose
        (Containing_Directory => Web_Dir,
         Name                 => "templates");
   begin
      Get_Content : declare
         Id : constant Positive := Positive'Value (Log_Id);
      begin
         return AWS.Response.Build
           (Content_Type => MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse
              (Filename     => Directories.Compose
                 (Containing_Directory => Web_Templates,
                  Name                 => "log",
                  Extension            => "thtml"),
               Translations => Database.Get_Log_Content (Id)));
      exception
         when Constraint_Error =>
            return Response.Build
              (MIME.Text_HTML, "<p>Wrong request</p>",
               Status_Code => Messages.S200);
      end Get_Content;
   end Show_Log;

   --------------------
   --  Show_Project  --
   --------------------

   function Show_Project (Request : in Status.Data) return Response.Data is
      Web_Dir       : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "htdocs");
      Web_Templates : constant String := Directories.Compose
        (Containing_Directory => Web_Dir,
         Name                 => "templates");
      URI           : constant String := Status.URI (Request);
      Project_Name  : constant String := URI (URI'First + 1 .. URI'Last);
      Configuration : constant Projects.Project_Config :=
                        Savadur.Remote_Files.Load_Project (Project_Name);
      Set           : Templates.Translate_Set :=
                        Savadur.Database.Get_Final_Status (Project_Name);
   begin
      Templates.Insert
        (Set, Templates.Assoc ("PROJECT_NAME", Project_Name));
      Templates.Insert
        (Set, Templates.Assoc
           ("PROJECT_DESCRIPTION",
            Projects.Desc_Utils.To_String (Configuration.Description)));

      Templates.Insert (Set, Savadur.Database.Get_Logs (Project_Name));

      return AWS.Response.Build
        (Content_Type => MIME.Text_HTML,
         Message_Body => AWS.Templates.Parse
           (Filename     => Directories.Compose
              (Containing_Directory => Web_Templates,
               Name                 => "project_page",
               Extension            => "thtml"),
            Translations => Set));
   end Show_Project;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Address := URL.Parse (Client_Service.URL);
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
