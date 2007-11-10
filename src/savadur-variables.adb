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

with Ada.Strings.Hash_Case_Insensitive;

package body Savadur.Variables is

   use Savadur.Utils;

   -------------
   -- Default --
   -------------

   function Default return Sets.Set is
      Default_Set : Sets.Set;
   begin
      Default_Set.Insert
        (New_Item =>
           Variable'(Name => Name_Utils.Value ("sources"),
                     Value => To_Unbounded_String ("sources")));

      return Default_Set;
   end Default;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Variable) return Containers.Hash_Type is
   begin
      return Hash (Key.Name);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Name) return Containers.Hash_Type is
   begin
      return Strings.Hash_Case_Insensitive (To_String (Key));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Var : in Variable) return String is
   begin
      return To_String (Var.Name) & " : " & To_String (Var.Value) & ASCII.LF;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Set : in Sets.Set) return String is
      Position : Sets.Cursor := Set.First;
      Result   : Unbounded_String := +"[" & ASCII.Lf;
   begin
      while Sets.Has_Element (Position) loop
         Append (Result, Image (Sets.Element (Position)));
         Sets.Next (Position);
      end loop;
      return -Result & "]";
   end Image;

   ---------
   -- Key --
   ---------

   function Key (Element : in Variable) return Name is
   begin
      return Element.Name;
   end Key;

end Savadur.Variables;
