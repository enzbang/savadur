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

with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.URL;
with SOAP.Dispatchers.Callback;

with Savadur.Server_Service.CB;
with Savadur.Logs;

package body Savadur.Web.Client is

   use AWS;
   use SOAP;

   HTTP       : AWS.Server.HTTP;
   Config     : AWS.Config.Object := AWS.Config.Get_Current;
   Dispatcher : Server_Service.CB.Handler;
   Address    : URL.Object;

   function HTTP_Callback (Request : in Status.Data) return Response.Data;
   --  Callbacks used for HTTP requests

   -------------------
   -- HTTP_Callback --
   -------------------

   function HTTP_Callback (Request : in Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Build (MIME.Text_HTML, "<p>Not found<p>", Messages.S404);
   end HTTP_Callback;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Address := URL.Parse (Server_Service.URL);
      AWS.Config.Set.Server_Port (Config, URL.Port (Address));

      Dispatcher := Dispatchers.Callback.Create
        (HTTP_Callback'Access, Server_Service.CB.SOAP_CB'Access);

      AWS.Server.Start (HTTP, Dispatcher, Config);

      Logs.Write
        (Content => "Server started on port " & URL.Port (Address),
         Kind    => Logs.Information);
   end Start;

end Savadur.Web.Client;
