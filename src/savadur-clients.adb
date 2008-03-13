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

with Ada.Strings.Hash_Case_Insensitive;

with Savadur.Logs;
with Savadur.Utils;

package body Savadur.Clients is

   use Savadur.Utils;

   function Image (Clients_Set : in Sets.Set) return String;
   --  Returns the Client_Set image

   subtype Set is Sets.Set;

   function Key (Client : in Clients.Client) return String;

   package Keys is new Sets.Generic_Keys
     (Key_Type        => String,
      Key             => Key,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   Registered : Sets.Set;

   -----------------------
   -- Callback_Endpoint --
   -----------------------

   function Callback_Endpoint (Position : in Cursor) return String is
            Element : constant Client := Sets.Element (Sets.Cursor (Position));
   begin
      return -Element.Callback_Endpoint;
   end Callback_Endpoint;

   -----------------
   -- Clients_Set --
   -----------------

   function Clients_Set return Templates.Translate_Set is
      use type Templates.Tag;
      P_Client : Sets.Cursor := Registered.First;
      Names    : Templates.Tag;
      Status   : Templates.Tag;
      Running  : Templates.Tag;
      Set      : Templates.Translate_Set;
   begin
      while Sets.Has_Element (P_Client) loop
         Get_Status : declare
            Element : Client := Sets.Element (P_Client);
         begin
            Names   := Names & Element.Key;
            Status  := Status & Client_Status'Image (Element.Status);
            Running := Running & Element.Running;
            Sets.Next (P_Client);
         end Get_Status;
      end loop;

      Templates.Insert
        (Set, Templates.Assoc ("CLIENTS_NAME", Names));

      Templates.Insert
        (Set, Templates.Assoc ("CLIENTS_STATUS", Status));

      Templates.Insert
        (Set, Templates.Assoc ("CLIENTS_RUNNING", Running));

      return Set;
   end Clients_Set;

   -----------------
   -- Clients_Set --
   -----------------

   function Clients_Set
     (Status : in Client_Status) return Templates.Translate_Set is
      use type Templates.Tag;
      P_Client     : Sets.Cursor := Registered.First;
      Clients_List : Templates.Tag;
      Set          : Templates.Translate_Set;
   begin
      while Sets.Has_Element (P_Client) loop
         Get_Status : declare
            Element : Client := Sets.Element (P_Client);
         begin
            if Element.Status = Status then
               Clients_List := Clients_List & Element.Key;
            end if;
            Sets.Next (P_Client);
         end Get_Status;
      end loop;

      Templates.Insert
        (Set, Templates.Assoc ("ONLINE_CLIENTS", Clients_List));

      return Set;
   end Clients_Set;

   ----------
   -- Find --
   ----------

   function Find (Key : in String) return Cursor is
   begin
      return Cursor (Keys.Find (Registered, Key));
   end Find;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : in Cursor) return Boolean is
   begin
      return Sets.Has_Element (Sets.Cursor (Position));
   end Has_Element;

   ----------
   -- Hash --
   ----------

   function Hash (Client : in Clients.Client) return Containers.Hash_Type is
   begin
      return Strings.Hash_Case_Insensitive (To_String (Client.Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Clients_Set : in Sets.Set) return String is
      Position : Sets.Cursor := Sets.First (Clients_Set);
      Result   : Unbounded_String;
   begin
      while Sets.Has_Element (Position) loop
         Append
           (Result, "* "
            & To_String (Sets.Element (Position).Key) & ASCII.LF);
         Append (Result, "[" & ASCII.LF);
         Append
           (Result,
            "Name => " & To_String (Sets.Element (Position).Key) & ASCII.LF);
         Append
           (Result, "Endpoint => " &
            To_String (Sets.Element (Position).Callback_Endpoint) & ASCII.LF);
         Append (Result, "]" & ASCII.LF);
         Sets.Next (Position);
      end loop;

      return -Result;
   end Image;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Status : in Client_Status;
      Action : in Iterate_Action)
   is

      procedure Iterate (Position : in Sets.Cursor);

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Position : in Sets.Cursor) is
         Element : constant Client := Sets.Element (Position);
      begin
         Logs.Write ("Status is " & Client_Status'Image (Status));

         Action.all (-Element.Key,
                     -Element.Server_Name,
                     -Element.Callback_Endpoint);
      end Iterate;

   begin
      Registered.Iterate (Iterate'Access);
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key (Client : in Clients.Client) return String is
   begin
      return -Client.Key;
   end Key;

   function Key (Position : in Cursor) return String is
      Element : constant Client := Sets.Element (Sets.Cursor (Position));
   begin
      return -Element.Key;
   end Key;

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (C1, C2 : in Client) return Boolean is
   begin
      return C1.Key = C2.Key;
   end Key_Equal;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key               : in String;
      Metadata          : in Clients.Metadata;
      Status            : in Client_Status;
      Server_Name       : in String;
      Callback_Endpoint : in String)
   is
   begin
      Insert_Or_Update : begin
         Clients.Registered.Insert
           (New_Item => Client'(+Key, Metadata, Status,
                                +Server_Name, +Callback_Endpoint,
                                Running  => <>));
      exception
         when Constraint_Error =>
            --  If the client has been deconnected and try to register again
            --  replace the old configuration by the new one
            Logs.Write ("Client exits... update it");
            Clients.Registered.Replace
              (New_Item => Client'(+Key, Metadata, Status,
                                   +Server_Name, +Callback_Endpoint,
                                   Running => <>));
      end Insert_Or_Update;

   end Register;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Key : in String; Status : in Client_Status; Message : in String := "") is
      Element : Client := Keys.Element (Registered, Key);
   begin
      Element.Status  := Status;
      Element.Running := +Message;
      Registered.Replace (Element);
   end Set_Status;

   -----------------
   -- Server_Name --
   -----------------

   function Server_Name (Position : in Cursor) return String is
      Element : constant Client := Sets.Element (Sets.Cursor (Position));
   begin
      return -Element.Server_Name;
   end Server_Name;

   ------------
   -- Status --
   ------------

   function Status (Position : in Cursor) return Client_Status is
      Element : constant Client := Sets.Element (Sets.Cursor (Position));
   begin
      return Element.Status;
   end Status;

end Savadur.Clients;
