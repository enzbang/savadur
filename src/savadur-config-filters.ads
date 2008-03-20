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

package Savadur.Config.Filters is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   Config_Error : exception renames Savadur.Config.Config_Error;

   type Filter_Id is new Unbounded_String;

   type Set is array (1 .. 2) of Filter_Id;

   Null_Set : constant Set;

   package Id_Utils is new Generic_Utils (Source => Filter_Id);

   type Pattern is new Unbounded_String;

   package Pattern_Utils is new Generic_Utils (Source => Pattern);

   type Filter is record
      Id      : Filter_Id;
      Pattern : Filters.Pattern;
   end record;

   Null_Filter : constant Filter;

   function Get (Id : in Filter_Id) return Filter;
   --  Returns the filter with the given Id or Null_Filter if not found

   procedure Start_Element
     (Prefix        : in String;
      Namespace_URI : in Unicode.CES.Byte_Sequence := "";
      Local_Name    : in Unicode.CES.Byte_Sequence := "";
      Qname         : in Unicode.CES.Byte_Sequence := "";
      Atts          : in Sax.Attributes.Attributes'Class);
   --  Parse a filter node and insert it into the filters set. The filter id is
   --  prefixed by the given prefix. This makes it possible to have filters
   --  defined in multiple XML document (project, SCM) with the same name.

   function Get_Id (Prefix, Id : in String) return Filter_Id;
   --  Returns the Filter_Id for the given filter id and the prefix

   function Simple_Name (Id : in Filter_Id) return String;
   --  Returns Id simple name, this is the Id without the leading prefix

private

   Null_Filter : constant Filter :=
                   Filter'(Id => Id_Utils.Nil, Pattern => Pattern_Utils.Nil);

   Null_Set : constant Set := Set'(others => Id_Utils.Nil);

end Savadur.Config.Filters;
