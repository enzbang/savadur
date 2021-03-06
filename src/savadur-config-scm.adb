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

with Savadur.Actions;
with Savadur.Config.Cmd;
with Savadur.Config.Filters;
with Savadur.Logs;
with Savadur.Utils;

package body Savadur.Config.SCM is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type Node_Value is (SCM, Name, Action, Cmd, Filter);

   type Attribute is (Id, Result, Skip_On_Init);

   type XML_Attribute is array (Attribute) of Boolean;

   type XML_Schema is array (Node_Value) of XML_Attribute;

   Schema : constant XML_Schema := XML_Schema'
     (SCM    => XML_Attribute'(Id            => False, others => False),
      Cmd    => XML_Attribute'(others => False),
      Action => XML_Attribute'(Id | Result | Skip_On_Init => True,
                               others                     => False),
      Name   => XML_Attribute'(Id            => True, others => False),
      Filter => XML_Attribute'(others => False));

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   function Internal_Parse (Filename : in String) return Savadur.SCM.SCM;
   --  Parses the given filename and returns the parsed SCM

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Content_Value : Unbounded_String;
      Action        : Actions.Action;
      SCM           : Savadur.SCM.SCM;
   end record;

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "");

   overriding procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence) is
   begin
      Append (Handler.Content_Value, To_Unbounded_String (Ch));
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
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
            Handler.SCM.Actions.Insert (New_Item => Handler.Action);
            Handler.Action := Actions.Null_Action;

         when Cmd =>
            Config.Cmd.End_Element
              (Handler.Action.Cmd, To_String (Handler.Content_Value));

         when SCM | Name | Filter =>
            null;
      end case;

      Handler.Content_Value := Null_Unbounded_String;
   end End_Element;

   ---------
   -- Get --
   ---------

   function Get (SCM_Name : in String) return Savadur.SCM.SCM is
      use Savadur.SCM.Id_Utils;
      C : Savadur.SCM.Sets.Cursor;
   begin
      C := Savadur.SCM.Keys.Find (Configurations, Value (SCM_Name));
      if Savadur.SCM.Sets.Has_Element (C) then
         return Savadur.SCM.Sets.Element (C);
      else
         raise Name_Error
           with "Try loading unknown scm " & SCM_Name;
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

      raise Config_Error with "(SCM) Unknown attribute " & S;
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

      raise Config_Error with "(SCM) Unknown node " & S;
   end Get_Node_Value;

   --------------------
   -- Internal_Parse --
   --------------------

   function Internal_Parse (Filename : in String) return Savadur.SCM.SCM is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      Reader.SCM :=
        Savadur.SCM.SCM'
          (Id       => Savadur.SCM.Id_Utils.Nil,
           Actions  => Actions.Sets.Empty_Set,
           Filename => +Filename);

      Input_Sources.File.Open
        (Filename => Filename,
         Input    => Source);

      Parse (Reader, Source);
      Input_Sources.File.Close (Source);

      return Reader.SCM;
   end Internal_Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      use Ada.Directories;

      S : Search_Type;
      D : Directory_Entry_Type;
   begin
      Start_Search
        (Search    => S,
         Directory => Config.SCM_Directory,
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
                        Kind    => Logs.Handler.Verbose);

            Configurations.Insert (Internal_Parse (Filename));
         exception
            when Config_Error =>
               Logs.Write
                 (Content => "Wrong SCM file : " & Filename,
                  Kind    => Logs.Handler.Error);
         end Load_Config;
      end loop Walk_Directories;
   exception
      when Name_Error =>
         raise Config_Error with " No SCM Directory ?";
   end Parse;

   ------------
   -- Reload --
   ------------

   procedure Reload (SCM_Name : in String; Filename : in String := "") is
   begin
      Try_Reload : declare
         Old_SCM  : aliased constant Savadur.SCM.SCM := Get (SCM_Name);
         Filename : constant String := -Old_SCM.Filename;
         New_SCM  : constant Savadur.SCM.SCM := Internal_Parse (Filename);
      begin
         Configurations.Include (New_SCM);
      end Try_Reload;

   exception
      when Name_Error =>

         --  SCM is new. Load the given filename

         if Filename = "" then
            raise Config_Error;
         end if;

         Load : declare
            New_SCM  : constant Savadur.SCM.SCM := Internal_Parse (Filename);
         begin
            Configurations.Include (New_SCM);
         end Load;
   end Reload;

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
      use Sax.Attributes;

      NV   : constant Node_Value := Get_Node_Value (Local_Name);
      Attr : Attribute;

   begin
      case NV is
         when Filter =>
            Filters.Start_Element
              (Prefix        =>
                 Savadur.SCM.Id_Utils.To_String (Handler.SCM.Id),
               Namespace_URI => Namespace_URI,
               Local_Name    => Local_Name,
               Qname         => Qname,
               Atts          => Atts);

         when Cmd =>
            Config.Cmd.Start_Element
              (Command       => Handler.Action.Cmd,
               Prefix        =>
                 Savadur.SCM.Id_Utils.To_String (Handler.SCM.Id),
               Namespace_URI => Namespace_URI,
               Local_Name    => Local_Name,
               Qname         => Qname,
               Atts          => Atts);

         when others =>
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
                        when SCM | Cmd | Filter =>
                           null;
                     end case;

                  when Result =>
                     case NV is
                        when Action =>
                           Handler.Action.Result :=
                             Actions.Result_Type'Value (Get_Value (Atts, J));

                        when Name | SCM | Cmd | Filter =>
                           null;
                     end case;

                  when Skip_On_Init =>
                     case NV is
                        when Action =>
                           Handler.Action.Skip_On_Init :=
                             Boolean'Value (Get_Value (Atts, J));

                        when Name | SCM | Cmd | Filter =>
                           null;
                     end case;
               end case;
            end loop;
      end case;
   end Start_Element;

end Savadur.Config.SCM;
