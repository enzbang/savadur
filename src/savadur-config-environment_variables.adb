------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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
with Ada.Directories;
with Ada.Strings.Unbounded;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

with Savadur.Utils;

package body Savadur.Config.Environment_Variables is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type Node_Value is (Environment_Variables, Project, Var);

   type Attribute is (Name, Value, Mode);

   type XML_Attribute is array (Attribute) of Boolean;

   type XML_Schema is array (Node_Value) of XML_Attribute;

   Schema : constant XML_Schema
     := XML_Schema'(Environment_Variables => XML_Attribute'(others => False),
                    Project               => XML_Attribute'(Name   => True,
                                                            others => False),
                    Var                   => XML_Attribute'(Name   => True,
                                                            Value  => True,
                                                            Mode   => True));

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Map : Savadur.Environment_Variables.Containers.Maps.Map;
   end record;

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

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

   function Parse
     (Project : access Projects.Project_Config)
      return Savadur.Environment_Variables.Containers.Maps.Map
   is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      if Directories.Exists (Projects.Project_Env_Filename (Project)) then
         --  Skip parsing without errors if file not found as having an env
         --  file is not required

         Input_Sources.File.Open
           (Filename => Projects.Project_Env_Filename (Project),
            Input    => Source);

         Parse (Reader, Source);

         Input_Sources.File.Close (Source);
      end if;
      return Reader.Map;
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
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);

      use Sax.Attributes;

      NV   : constant Node_Value := Get_Node_Value (Local_Name);
      Attr : Attribute;
   begin

      case NV is
         when Var =>
            New_Var : declare
               Var_Name : Unbounded_String;
               Var      : Savadur.Environment_Variables.Var;
            begin
               for J in 0 .. Get_Length (Atts) - 1 loop
                  Attr := Get_Attribute (Get_Qname (Atts, J));
                  if not Schema (NV) (Attr) then
                     raise Config_Error with "Unknow attribute "
                       & Node_Value'Image (NV) & "." & Get_Qname (Atts, J);
                  end if;

                  case Attr is
                     when Name  => Var_Name  := +Get_Value (Atts, J);
                     when Value => Savadur.Environment_Variables.Set_Value
                          (Var, Get_Value (Atts, J));
                     when Mode  =>
                        Savadur.Environment_Variables.Set_Action
                          (Var,
                           Savadur.Environment_Variables.
                             Var_Action'Value (Get_Value (Atts, J)));
                  end case;
               end loop;

               Savadur.Environment_Variables.Containers.Maps.Insert
                 (Container => Handler.Map,
                  Key       => -Var_Name,
                  New_Item  => Var);
            end New_Var;

         when Environment_Variables | Project =>
            null;
      end case;
   end Start_Element;

end Savadur.Config.Environment_Variables;
