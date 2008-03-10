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

with Ada.Strings.Unbounded;

with Sax.Attributes;
with Unicode.CES;

with Savadur.Utils;

package Savadur.Config.Cmd is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type External_Command is new Unbounded_String;
   package External_Command_Utils is
     new Generic_Utils (Source => External_Command);

   type Output_Pattern is new Unbounded_String;
   package Output_Pattern_Utils is
     new Generic_Utils (Source => Output_Pattern);

   type Command is record
      Cmd    : External_Command;
      Output : Output_Pattern;
   end record;

   Null_Command : constant Command;

   procedure Start_Element
     (Command       : in out Cmd.Command;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);
   --  Parse a cmd node and set Command accordingly. This is expected
   --  to be called from the Start_Element Sax callback.

   procedure End_Element
     (Command : in out Cmd.Command;
      Content : in     String);
   --  Expected to be called inside the End_Element Sax callback. Content is
   --  the value of the tag (i.e. value between the opening and closing tag).

private

   Null_Command : constant Command :=
                    (External_Command_Utils.Nil, Output_Pattern_Utils.Nil);

end Savadur.Config.Cmd;
