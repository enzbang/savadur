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

   ---------
   -- "+" --
   ---------

   function "+" (Source : Id) return U_Id is
   begin
      return U_Id (+String (Source));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Source : U_Id) return Id is
   begin
      return Id (-Unbounded_String (Source));
   end "-";

   ----------
   -- Hash --
   ----------

   function Hash (Key : Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (SCM_Map : Maps.Map) return String is
      Position : Maps.Cursor := Maps.First (SCM_Map);
      Result  : Unbounded_String;
   begin
      while Maps.Has_Element (Position) loop
         Append (Result, "* " & String (Maps.Key (Position)) & ASCII.Lf);
         Append (Result,
                 Savadur.Action.Image (Maps.Element (Position).Actions));
         Maps.Next (Position);
      end loop;

      return -Result;

   end Image;

end Savadur.SCM;
