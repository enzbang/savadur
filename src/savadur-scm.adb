------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
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

package body Savadur.SCM is

   ----------
   -- Hash --
   ----------

   function Hash (Key : in SCM) return Containers.Hash_Type is
   begin
      return Hash (Key.Id);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (SCM : in Savadur.SCM.SCM) return String is
   begin
      return To_String (SCM.Id) & ASCII.LF
        & Actions.Image (SCM.Actions);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Set : in Sets.Set) return String is
      Position : Sets.Cursor := Sets.First (Set);
      Result  : Unbounded_String;
   begin
      while Sets.Has_Element (Position) loop
         Append (Result, "* " & Image (Sets.Element (Position)));
         Sets.Next (Position);
      end loop;

      return -Result;
   end Image;

   ---------
   -- Key --
   ---------

   function Key (Element : in SCM) return Id is
   begin
      return Element.Id;
   end Key;

end Savadur.SCM;
