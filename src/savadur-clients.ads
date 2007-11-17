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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Unbounded;

with Savadur.Web_Services.Client;

package Savadur.Clients is

   use Ada;
   use Ada.Strings.Unbounded;

   subtype Metadata is Web_Services.Client.Metadata;

   type Client is record
      Key               : Unbounded_String;
      Metadata          : Clients.Metadata;
      Callback_Endpoint : Unbounded_String;
   end record;

   function Hash (Client : in Clients.Client) return Containers.Hash_Type;
   --  Renames Strings.Hash

   function Key_Equal (C1, C2 : in Client) return Boolean;

   Emtpy_Client : constant Client;

   ----------
   -- Sets --
   ----------

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Client,
      Hash                => Hash,
      Equivalent_Elements => Key_Equal);

   function Image (Clients_Set : in Sets.Set) return String;
   --  Returns the Client_Set image

   Registered : Sets.Set;

private

   Emtpy_Client : constant Client :=
                    (Key => <>, Metadata => <>, Callback_Endpoint => <>);

end Savadur.Clients;
