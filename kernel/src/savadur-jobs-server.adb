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

with Savadur.Clients;
with Savadur.Logs;
with Savadur.Project_List;
with Savadur.Server_Service.Client;
with Savadur.Signed_Files;
with Savadur.Utils;
with Savadur.Web_Services.Server;

package body Savadur.Jobs.Server is

   use Savadur.Utils;

   ---------
   -- Run --
   ---------

   function Run
     (Project  : access Projects.Project_Config;
      Env_Var  : in     Environment_Variables.Maps.Map;
      Scenario : in     Scenarios.Id) return Boolean
   is
      pragma Unreferenced (Env_Var);

      use Projects.Id_Utils;
      use Scenarios.Id_Utils;

      procedure Send_Job_Request (Position : in Project_List.Clients.Cursor);
      --  Sends the job request to the client

      ----------------------
      -- Send_Job_Request --
      ----------------------

      procedure Send_Job_Request (Position : in Project_List.Clients.Cursor) is
         Client_Ref : constant Project_List.Client :=
                        Project_List.Clients.Element (Position);
         P_Client   : constant Clients.Sets.Cursor :=
                        Clients.Keys.Find
                          (Clients.Registered, -Client_Ref.Key);
      begin
         if Clients.Sets.Has_Element (P_Client) then
            declare
               Client : constant Clients.Client :=
                          Clients.Sets.Element (P_Client);
            begin
               Logs.Write ("Send job request to " & (-Client.Key));
               Server_Service.Client.Run
                 (Scenario       => -Scenario,
                  Signed_Project =>
                    Web_Services.Server.Signed_Project
                      (Signed_Files.To_External_Handler (Project.Signature)),
                  Endpoint       => -Client.Callback_Endpoint);
            end;

         else
            Logs.Write
              (Content => "Cannot found client '" & (-Client_Ref.Key) & ''');
         end if;
      end Send_Job_Request;

      Clients : constant Project_List.Clients.Vector :=
                  Project_List.Get_Clients (-Project.Project_Id, -Scenario);

   begin
      Clients.Iterate (Send_Job_Request'Access);
      return True;
   end Run;

end Savadur.Jobs.Server;