------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
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

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

with Savadur.Utils;
with Savadur.Action;
with Savadur.Scenario;

package body Savadur.Config.SCM is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Config_Error : exception;

   type Node_Value is (SCM, Name, Action, Cmd);

   type Attribute is (Id);

   function Get_Node_Value (S : String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : String) return Attribute;
   --  Returns the attributed value matching the given string or raise
   --  Config_Error

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Value           : Unbounded_String;
      Action          : Savadur.Action.Action;
      Action_Id       : Unbounded_String;
      Scenario        : Savadur.Scenario.Scenario;
      SCM_Id          : Savadur.SCM.U_Id;
      Inside_Scenario : Boolean := False;
      SCM             : Savadur.SCM.SCM;
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
      Append (Handler.Value, To_Unbounded_String (Ch));
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
            if Handler.Action_Id = "" then
               raise Config_Error with " Null action id !";
            end if;

            Handler.SCM.Actions.Insert
              (Key      => Savadur.Action.Id (-Handler.Action_Id),
               New_Item => Handler.Action);
         when Cmd =>
            Handler.Action.Cmd := Savadur.Action.Command (Handler.Value);
         when SCM | Name =>
            null;
      end case;

      Handler.Value := Null_Unbounded_String;

   end End_Element;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (S : String) return Attribute is
      Upper_S : String := S;
      use GNAT;
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
      Upper_S : String := S;
      use GNAT;
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

   procedure Parse (SCM_Dir : in String) is
      use Ada.Directories;

      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;

      S : Search_Type;
      D : Directory_Entry_Type;
   begin
      Ada.Text_IO.Put_Line (SCM_Dir);
      Start_Search
        (Search    => S,
         Directory => SCM_Dir,
         Pattern   => "*.xml",
         Filter    => Filter_Type'(Ordinary_File => True,
                                   Directory     => False,
                                   Special_File  => False));

      Walk_Directories : while More_Entries (S) loop
         Get_Next_Entry (S, D);
         Load_Config : declare
            Filename : constant String := Full_Name (D);
         begin
            Ada.Text_IO.Put_Line (Filename);
            Reader.SCM :=
              Savadur.SCM.SCM'(Actions => Savadur.Action.Maps.Empty_Map);

            Input_Sources.File.Open
              (Filename => Filename,
               Input    => Source);
            Parse (Reader, Source);
            Input_Sources.File.Close (Source);

            Savadur.SCM.Maps.Insert
              (Container => Configurations,
               Key       => Savadur.SCM.Id
                 (To_String (Unbounded_String (Reader.SCM_Id))),
               New_Item  => Reader.SCM);

         end Load_Config;
      end loop Walk_Directories;
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
      Attr : Attribute;
      NV   : constant Node_Value := Get_Node_Value (Local_Name);

   begin
      case NV is
         when Name =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Id =>
                     Handler.SCM_Id :=
                       Savadur.SCM.U_Id (+Get_Value (Atts, J));
               end case;
            end loop;
         when Action =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Id =>
                     Handler.Action_Id := +Get_Value (Atts, J);
               end case;
            end loop;
         when Cmd | Scm =>
            null;
      end case;
   end Start_Element;

end Savadur.Config.SCM;
