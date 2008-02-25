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

with Ada.Strings.Hash_Case_Insensitive;

package body Savadur.Scenarios is

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Scenario) return Containers.Hash_Type is
   begin
      return Hash (Key.Id);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash_Case_Insensitive (To_String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Scenario : in Scenarios.Scenario) return String is
   begin
      return "* " & To_String (Scenario.Id)
        & ASCII.LF & Savadur.Actions.Image (Scenario.Actions)
        & ASCII.LF & "periodic = "
        & Savadur.Times.Image (Scenario.Periodic);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Scenarios : in Sets.Set) return String is
      Result : Unbounded_String;

      procedure Image (Position : in Sets.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Sets.Cursor) is
      begin
         Append (Result, Image (Sets.Element (Position)));
      end Image;

   begin
      Sets.Iterate (Container => Scenarios,
                    Process   => Image'Access);
      return -Result;
   end Image;

   ---------
   -- Key --
   ---------

   function Key (Element : in Scenario) return Id is
   begin
      return Element.Id;
   end Key;

end Savadur.Scenarios;
