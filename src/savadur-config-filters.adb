------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;

with Sax.Attributes;

with Savadur.Logs;

package body Savadur.Config.Filters is

   type Attribute is (Id, Regexp);

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   function Hash (Key : in Filter_Id) return Containers.Hash_Type;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (S : in String) return Attribute is
      Upper_S : constant String := Ada.Characters.Handling.To_Upper (S);
   begin
      for SA in Attribute'Range loop
         if Attribute'Image (SA) = Upper_S then
            return SA;
         end if;
      end loop;

      raise Config_Error with "(Filters) Unknown attribute " & S;
   end Get_Attribute;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Filter_Id) return Containers.Hash_Type is
   begin
      return Strings.Unbounded.Hash (Id_Utils.To_Unbounded_String (Key));
   end Hash;

   package Filters_Map is
     new Containers.Hashed_Maps (Filter_Id, Filter, Hash, "=");

   Maps : Filters_Map.Map;

   ---------
   -- Get --
   ---------

   function Get (Id : in Filter_Id) return Filter is
   begin
      if Maps.Contains (Id) then
         return Maps.Element (Id);
      else
         return Null_Filter;
      end if;
   end Get;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Prefix, Id : in String) return Filter_Id is
   begin
      return Id_Utils.Value (Prefix & "$" & Id);
   end Get_Id;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Id : in Filter_Id) return String is
      I : constant String := Id_Utils.To_String (Id);
      K : constant Natural := Strings.Fixed.Index (I, "$");
   begin
      if K = 0 then
         Logs.Write
           ("(Filters) wrong If received in Simple_Name: " & I,
            Logs.Handler.Error);
         raise Constraint_Error
           with "(Filters) wrong If received in Simple_Name: " & I;

      else
         return I (K + 1 .. I'Last);
      end if;
   end Simple_Name;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Prefix        : in String;
      Namespace_URI : in Unicode.CES.Byte_Sequence := "";
      Local_Name    : in Unicode.CES.Byte_Sequence := "";
      Qname         : in Unicode.CES.Byte_Sequence := "";
      Atts          : in Sax.Attributes.Attributes'Class)
   is
      use Sax.Attributes;

      Attr : Attribute;
      F    : Filter;
   begin
      for J in 0 .. Get_Length (Atts) - 1 loop
         Attr := Get_Attribute (Get_Qname (Atts, J));

         case Attr is
            when Id =>
               F.Id := Get_Id (Prefix, Get_Value (Atts, J));
            when Regexp =>
               F.Pattern := Pattern_Utils.Value (Get_Value (Atts, J));
         end case;
      end loop;

      if F.Id = Id_Utils.Nil then
         Logs.Write ("(Filters) missing Id attribute", Logs.Handler.Error);
      end if;

      if F.Pattern = Pattern_Utils.Nil then
         Logs.Write
           ("(Filters) missing Pattern attribute", Logs.Handler.Error);
      end if;

      Maps.Insert (F.Id, F);
   end Start_Element;

end Savadur.Config.Filters;
