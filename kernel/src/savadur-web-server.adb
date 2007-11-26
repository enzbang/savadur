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

with IO_Exceptions;

with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Server;
with AWS.Status;
with AWS.URL;

with SOAP.Dispatchers.Callback;

with Savadur.Client_Service.CB;
with Savadur.Config.Project;
with Savadur.Jobs.Server;
with Savadur.Projects;
with Savadur.Signed_Files;
with Savadur.Logs;

package body Savadur.Web.Server is

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

   -------------------
   -- HTTP_Callback --
   -------------------

   function HTTP_Callback (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      Logs.Write (Content => "Calling => " & URI, Kind => Logs.Very_Verbose);
      if URI = "/run" then
         return Run (Request);
      end if;

      return Response.Build
        (MIME.Text_HTML, "<p>" & URI & " not found</p>", Messages.S404);
   end HTTP_Callback;

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
         Kind    => Logs.Information);
   end Start;

end Savadur.Web.Server;
