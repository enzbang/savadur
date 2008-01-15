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
with Ada.Strings.Unbounded;

package Savadur.Servers is

   use Ada;
   use Ada.Strings.Unbounded;

   type Server is record
      Name : Unbounded_String;
      URL  : Unbounded_String;
   end record;

   function Hash (Server : in Servers.Server) return Containers.Hash_Type;
   --  Renames Strings.Hash (on server name)

   function Key_Equal (S1, S2 : in Server) return Boolean;

   Empty_Server : constant Server;

   ----------
   -- Sets --
   ----------

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Server,
      Hash                => Hash,
      Equivalent_Elements => Key_Equal);

   function Image (Servers_Set : in Sets.Set) return String;
   --  Returns the Servers_Set image

private

   Empty_Server : constant Server :=
                    (Name => Null_Unbounded_String,
                     Url  => Null_Unbounded_String);

end Savadur.Servers;
