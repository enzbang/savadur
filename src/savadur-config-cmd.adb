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

   procedure Start_Element
     (Command       : in out Cmd.Command;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class)
   is
   begin
      for J in 0 .. Get_Length (Atts) - 1 loop
         Handle_Attribute : declare
            A : constant String := To_Lower (Get_Qname (Atts, J));
            V : constant String := Get_Value (Atts, J);
         begin
            if A = "regexp" and then V /= "" then
               Command.Output := Output_Pattern_Utils.Value (V);
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
