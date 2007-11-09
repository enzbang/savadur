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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;

package Savadur.Servers is

   use Ada;
   use Ada.Strings.Unbounded;

   type Id is new String;

   type Server is record
      Name : Unbounded_String;
      URL  : Unbounded_String;
   end record;

   function Hash (Key : in Id) return Containers.Hash_Type;
   --  Renames Strings.Hash

   Emtpy_Server : constant Server;

   ----------
   -- Maps --
   ----------

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Id,
      Element_Type    => Server,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Image (Servers_Map : in Maps.Map) return String;
   --  Return the Servers_Map image

private

   Emtpy_Server : constant Server :=
                    (Null_Unbounded_String, Null_Unbounded_String);

end Savadur.Servers;
