------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2008                             --
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;

with Sax.Readers;
with Sax.Attributes;
with Input_Sources.File;
with Unicode.CES;

with Savadur.Utils;

package body Savadur.Config.Committers is

   use Ada;
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   package Name_Mail is new Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   Map : Name_Mail.Map;

   type Node_Value is (Committers, Set);

   type Attribute is (Name, Value);

   type Tree_Reader is new Sax.Readers.Reader with null record;

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   procedure Parse;
   --  Fills the Set with committers e-mails

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   ---------
   -- Get --
   ---------

   function Get (Name : in String) return String is
   begin
      if Map.Contains (Name) then
         return Map.Element (Name);

      elsif Map.Contains ("*")
        and then Strings.Fixed.Index (Name, "@") = 0
      then
         return Name & Map.Element ("*");

      else
         return Name;
      end if;
   end Get;

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

      raise Config_Error with "Unknown node " & S;
   end Get_Attribute;

   --------------------
   -- Get_Node_Value --
   --------------------

   function Get_Node_Value (S : in String) return Node_Value is
      Upper_S : constant String := Ada.Characters.Handling.To_Upper (S);
   begin
      for NV in Node_Value'Range loop
         if Node_Value'Image (NV) = Upper_S then
            return NV;
         end if;
      end loop;

      raise Config_Error with "Unknown node " & S;
   end Get_Node_Value;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      Filename : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "committers",
         Extension            => "xml");
      Reader   : Tree_Reader;
      Source   : Input_Sources.File.File_Input;
   begin
      if Directories.Exists (Filename) then
         Input_Sources.File.Open
           (Filename => Filename,
            Input    => Source);

         Parse (Reader, Source);

         Input_Sources.File.Close (Source);
      end if;
   end Parse;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Handler);
      pragma Unreferenced (Qname);

      use Sax.Attributes;
      NV   : constant Node_Value := Get_Node_Value (Local_Name);
      Attr : Attribute;

   begin
      case NV is
         when Committers =>
            null;

         when Set =>
            Get_Name_Value : declare
               A_Name, A_Value : Unbounded_String;
            begin
               for J in 0 .. Get_Length (Atts) - 1 loop
                  Attr := Get_Attribute (Get_Qname (Atts, J));

                  case Attr is
                     when Name =>
                        A_Name := +Get_Value (Atts, J);

                     when Value =>
                        A_Value := +Get_Value (Atts, J);
                  end case;
               end loop;

               if Map.Contains (-A_Name) then
                  Map.Replace (-A_Name, -A_Value);
               else
                  Map.Insert (-A_Name, -A_Value);
               end if;
            end Get_Name_Value;
      end case;
   end Start_Element;

begin --  BEGIN Savadur.Config.Committers
   Parse;
end Savadur.Config.Committers;
