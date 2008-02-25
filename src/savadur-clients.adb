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

with Ada.Strings.Hash;

with Savadur.Utils;

package body Savadur.Clients is

   use Savadur.Utils;

   ----------
   -- Hash --
   ----------

   function Hash (Client : in Clients.Client) return Containers.Hash_Type is
   begin
      return Strings.Hash (To_String (Client.Key));
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

   ---------
   -- Key --
   ---------

   function Key (Client : in Clients.Client) return String is
   begin
      return -Client.Key;
   end Key;

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (C1, C2 : in Client) return Boolean is
   begin
      return C1.Key = C2.Key;
   end Key_Equal;

end Savadur.Clients;
