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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;

with AWS.Templates;

with Savadur.Web_Services.Client;

package Savadur.Clients is

   use Ada;
   use Ada.Strings.Unbounded;

   use AWS;

   subtype Metadata is Web_Services.Client.Metadata;

   type Client_Status is (Idle, Busy, Offline);

   type Client is private;

   Empty_Client : constant Client;

   procedure Register
     (Key               : in String;
      Metadata          : in Clients.Metadata;
      Status            : in Client_Status;
      Server_Name       : in String;
      Callback_Endpoint : in String);
   --  Register a new client

   procedure Set_Status (Key : in String; Status : in Client_Status);
   --  Update the client status

   function Clients_Set return Templates.Translate_Set;
   --  Returns a set of client data

   function Clients_Set
     (Status : in Client_Status) return Templates.Translate_Set;
   --  Returns a set of client in given status

   type Iterate_Action is access procedure
     (Key : in String; Server_Name : in String; Callback_Endpoint : in String);

   procedure Iterate
     (Status : in Client_Status;
      Action : in Iterate_Action);
   --  Run the action on all client with the given status

   type Cursor is private;

   No_Element : constant Cursor;

   function Find (Key : in String) return Cursor;
   function Has_Element (Position : in Cursor) return Boolean;

   function Key (Position : in Cursor) return String;
   function Status (Position : in Cursor) return Client_Status;
   function Server_Name (Position : in Cursor) return String;
   function Callback_Endpoint (Position : in Cursor) return String;

private
   type Client is record
      Key               : Unbounded_String;
      Metadata          : Clients.Metadata;
      Status            : Client_Status;
      Server_Name       : Unbounded_String;
      Callback_Endpoint : Unbounded_String;
   end record;

   Empty_Client : constant Client :=
     (Key               => <>,
      Metadata          => <>,
      Status            => Offline,
      Server_Name       => <>,
      Callback_Endpoint => <>);

   function Hash (Client : in Clients.Client) return Containers.Hash_Type;
   --  Renames Strings.Hash

   function Key_Equal (C1, C2 : in Client) return Boolean;

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Client,
      Hash                => Hash,
      Equivalent_Elements => Key_Equal);

   type Cursor is new Sets.Cursor;

   No_Element : constant Cursor := Cursor (Sets.No_Element);

end Savadur.Clients;
