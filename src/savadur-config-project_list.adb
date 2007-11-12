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

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;
with Input_Sources.File;
with Unicode.CES;

with Savadur.Utils;

package body Savadur.Config.Project_List is

   use Ada;
   use Savadur.Utils;
   use Ada.Strings.Unbounded;

   type Node_Value is (Project_List, Project, Scenario, Client);

   type Attribute is (Id, Key);

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   type Tree_Reader is new Sax.Readers.Reader with record
      Project  : Unbounded_String;
      Scenario : Unbounded_String;
   end record;

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (S : in String) return Attribute is
      use GNAT;
      Upper_S : String := S;
   begin
      Case_Util.To_Upper (Upper_S);

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
      use GNAT;
      Upper_S : String := S;
   begin
      Case_Util.To_Upper (Upper_S);

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
      use Ada.Directories;

      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;

      S : Search_Type;
      D : Directory_Entry_Type;
   begin
      Start_Search
        (Search    => S,
         Directory =>
           Directories.Compose (Config.Savadur_Directory, "config"),
         Pattern   => "project_list.xml",
         Filter    => Filter_Type'(Ordinary_File => True,
                                   Directory     => False,
                                   Special_File  => False));

      Walk_Directories : while More_Entries (S) loop
         Get_Next_Entry (S, D);
         Load_Config : declare
            Filename : constant String := Full_Name (D);
         begin
            Text_IO.Put_Line (Filename);

            Input_Sources.File.Open (Filename => Filename, Input => Source);
            Parse (Reader, Source);
            Input_Sources.File.Close (Source);
         end Load_Config;
      end loop Walk_Directories;
   exception
      when IO_Exceptions.Name_Error =>
         raise Config_Error with " No Servers Directory ?";
   end Parse;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);

      use Sax.Attributes;
      NV   : constant Node_Value := Get_Node_Value (Local_Name);
      Attr : Attribute;

   begin
      case NV is
         when Project_List =>
            Handler.Project := Null_Unbounded_String;
            Handler.Scenario := Null_Unbounded_String;

         when Project =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Id =>
                     Handler.Project := +Get_Value (Atts, J);
                  when Key =>
                     raise Config_Error with "Unexpected Key attribute";
               end case;
            end loop;

         when Scenario =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Id =>
                     Handler.Scenario := +Get_Value (Atts, J);
                  when Key =>
                     raise Config_Error with "Unexpected Key attribute";
               end case;
            end loop;

         when Client =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Key =>
                     null;
                  when Id =>
                     raise Config_Error with "Unexpected Id attribute";
               end case;
            end loop;
      end case;
   end Start_Element;

end Savadur.Config.Project_List;
