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

package body Savadur.Config.Cmd is

   use Ada.Characters.Handling;
   use Sax.Attributes;

   type Attribute is (Regexp, Filter1, Filter2);

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

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

      raise Config_Error with "(Cmd) Unknown attribute " & S;
   end Get_Attribute;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Command       : in out Cmd.Command;
      Prefix        : in     String;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class) is
   begin
      for J in 0 .. Get_Length (Atts) - 1 loop
         Handle_Attribute : declare
            Att : constant Attribute := Get_Attribute (Get_Qname (Atts, J));
            Val : constant String := Get_Value (Atts, J);
         begin
            if Val /= "" then
               case Att is
                  when Regexp =>
                     Command.Output := Output_Pattern_Utils.Value (Val);

                  when Filter1 =>
                     Command.Filters (1) := Filters.Get_Id (Prefix, Val);

                  when Filter2 =>
                     Command.Filters (2) := Filters.Get_Id (Prefix, Val);
               end case;
            end if;
         end Handle_Attribute;
      end loop;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Command : in out Cmd.Command;
      Content : in     String) is
   begin
      Command.Cmd := External_Command_Utils.Value (Content);
   end End_Element;

end Savadur.Config.Cmd;
