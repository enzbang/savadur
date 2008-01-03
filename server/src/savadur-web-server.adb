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
with Savadur.Config.Project;
with Savadur.Config.Project_List;
with Savadur.Database;
with Savadur.Jobs.Server;
with Savadur.Projects;
with Savadur.Project_List;
with Savadur.Signed_Files;
with Savadur.Logs;

package body Savadur.Web.Server is

   use Ada;
   use AWS;
   use SOAP;

   HTTP          : AWS.Server.HTTP;
   Config        : AWS.Config.Object := AWS.Config.Get_Current;
   Dispatcher    : Client_Service.CB.Handler;
   Address       : URL.Object;

   function HTTP_Callback (Request : in Status.Data) return Response.Data;
   --  Callbacks used for HTTP requests

   function Run (Request : in Status.Data) return Response.Data;
   --  Runs a project

   function List (Request : in Status.Data) return Response.Data;
   --  Displays the project list

   function Show_Project (Request : in Status.Data) return Response.Data;
   --  Displays the project status

   -------------------
   -- HTTP_Callback --
   -------------------

   function HTTP_Callback (Request : in Status.Data) return Response.Data is
      Web_Dir       : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "htdocs");
      Web_CSS       : constant String := Directories.Compose
        (Containing_Directory => Web_Dir,
         Name                 => "css");
      URI           : constant String := Status.URI (Request);
   begin
      Logs.Write
        (Content => "Calling => " & URI,
         Kind    => Logs.Handler.Very_Verbose);
      if URI = "/" then
         return List (Request);
      elsif URI = "/run" then
         return Run (Request);
      elsif Savadur.Config.Project.Is_Project_Name
        (URI (URI'First + 1 .. URI'Last)) then
         return Show_Project (Request);
      elsif URI'Length > 4
        and then URI (URI'First .. URI'First + 3) = "/css"
      then
         Get_CSS : declare
            CSS_File : constant String := Directories.Compose
              (Containing_Directory => Web_CSS,
               Name                 => URI (URI'First + 5 .. URI'Last));
         begin
            if Directories.Exists (CSS_File) then
               Logs.Write (CSS_File);
               return Response.File
                 (MIME.Text_CSS, CSS_File);
            else
               return Response.Build
                 (MIME.Text_HTML,
                  "<p>File " & CSS_File & " not found</p>", Messages.S404);
            end if;
         end Get_CSS;
      end if;

      return Response.Build
        (MIME.Text_HTML, "<p>" & URI & " not found</p>", Messages.S404);
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

         Jobs.Server.Queue.Add (Signed_Project, Scenario_Id);

         return Response.Build
           (MIME.Text_HTML,
            "<p>Running " & Project_Name & "...</p>",
            Messages.S404);

      exception
         when IO_Exceptions.Name_Error =>
            return Response.Build
              (MIME.Text_HTML,
               "<p>Project " & Project_Name & " Not found</p>",
               Messages.S404);
      end Run_Project;
   end Run;

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
      URI          : constant String := Status.URI (Request);
      Project_Name : constant String := URI (URI'First + 1 .. URI'Last);
      Set          : Templates.Translate_Set :=
        Savadur.Database.Get_Final_Status (Project_Name);
   begin

      Templates.Insert (Set,
                        Templates.Assoc ("PROJECT_NAME", Project_Name));

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
