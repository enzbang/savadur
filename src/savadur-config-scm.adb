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

with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

with Savadur.Actions;
with Savadur.Logs;

package body Savadur.Config.SCM is

   use Ada;
   use Ada.Strings.Unbounded;

   type Node_Value is (SCM, Name, Action, Cmd);

   type Attribute is (Id, Result);

   type XML_Attribute is array (Attribute) of Boolean;

   type XML_Schema is array (Node_Value) of XML_Attribute;

   Schema : constant XML_Schema :=
              XML_Schema'(SCM    => XML_Attribute'(Id => False,
                                                   others => False),
                          Cmd    => XML_Attribute'(Id     => False,
                                                   others => False),
                          Action => XML_Attribute'(Id     => True,
                                                   Result => True),
                          Name   => XML_Attribute'(Id     => True,
                                                   others => False));

   function Get_Node_Value (S : in String) return Node_Value;

   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Content_Value : Unbounded_String;
      Action        : Actions.Action;
      SCM           : Savadur.SCM.SCM;
   end record;

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "");

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence) is
   begin
      Append (Handler.Content_Value, To_Unbounded_String (Ch));
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);
      NV : constant Node_Value := Get_Node_Value (Local_Name);
   begin
      case NV is
         when Action =>
            Handler.SCM.Actions.Insert
              (New_Item => Handler.Action);

         when Cmd =>
            Handler.Action.Cmd := Actions.Command (Handler.Content_Value);

         when SCM | Name =>
            null;
      end case;

      Handler.Content_Value := Null_Unbounded_String;
   end End_Element;

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
         Directory => Directories.Compose
           (Containing_Directory => Config.Savadur_Directory,
            Name                 => "scm"),
         Pattern   => "*.xml",
         Filter    => Filter_Type'(Ordinary_File => True,
                                   Directory     => False,
                                   Special_File  => False));

      Walk_Directories : while More_Entries (S) loop
         Get_Next_Entry (S, D);
         Load_Config : declare
            Filename : constant String := Full_Name (D);
         begin
            Logs.Write (Content => "Read SCM config file : " & Filename,
                        Kind    => Logs.Verbose);

            Reader.SCM :=
              Savadur.SCM.SCM'(Id      => Savadur.SCM.Id_Utils.Nil,
                               Actions => Actions.Sets.Empty_Set);

            Input_Sources.File.Open
              (Filename => Filename,
               Input    => Source);

            Parse (Reader, Source);
            Input_Sources.File.Close (Source);

            Savadur.SCM.Sets.Insert
              (Container => Configurations,
               New_Item  => Reader.SCM);

         end Load_Config;
      end loop Walk_Directories;
   exception
      when IO_Exceptions.Name_Error =>
         raise Config_Error with " No SCM Directory ?";
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
      for J in 0 .. Get_Length (Atts) - 1 loop
         Attr := Get_Attribute (Get_Qname (Atts, J));
         if not Schema (NV) (Attr) then
            raise Config_Error with "Unknow attribute "
              & Node_Value'Image (NV) & "." & Get_Qname (Atts, J);
         end if;

         case Attr is
            when Id =>
               case NV is
                  when Action =>
                     Handler.Action.Id :=
                       Actions.Id_Utils.Value (Get_Value (Atts, J));
                  when Name =>
                     Handler.SCM.Id :=
                       Savadur.SCM.Id_Utils.Value (Get_Value (Atts, J));
                  when SCM | Cmd => null;
               end case;

            when Result =>
               case NV is
                  when Action =>
                     Handler.Action.Result :=
                       Actions.Result_Type'Value (Get_Value (Atts, J));

                  when others => null;
               end case;
         end case;
      end loop;
   end Start_Element;

end Savadur.Config.SCM;
