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

with Ada.Directories;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

package body Savadur.Config.Project is

   use Ada;

   use Savadur.Utils;

   Config_Error : exception;

   type Node_Value is
     (SCM, Variable, SCM_Action, Action, Scenario, Cmd, Project, Name);

   type Attribute is (Id, Check, Value, Result);

   type XML_Attribute is array (Attribute) of Boolean;

   XML_Schema : constant array (Node_Value) of XML_Attribute :=
                  (SCM        =>  (Id     => True,
                                   others => False),
                   Variable   =>  (Id     => True,
                                   Value  => True,
                                   others => False),
                   SCM_Action => (Id     => True,
                                  Check  => True,
                                  Value  => True,
                                  others => False),
                   Action     => (Id     => True,
                                  Check  => True,
                                  Value  => True,
                                  others => False),
                   Scenario   => (Id     => True,
                                  others => False),
                   Cmd        => (others => False),
                   Project    => (others => False),
                   Name       => (Id     => True,
                                  others => False));

   function Get_Node_Value (S : String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : String) return Attribute;
   --  Returns the attributed value matching the given string or raise
   --  Config_Error

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Content_Value   : Unbounded_String;
      Var             : Variables.Variable;
      Action          : Actions.Action;
      Ref_Action      : Actions.Ref_Action;
      Scenario        : Scenarios.Scenario;
      Inside_Scenario : Boolean := False;
      Current_Project : Project_Config;
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
         when Variable =>
            --  Overwrite default values

            Handler.Current_Project.Variables.Include
              (New_Item => Handler.Var);

         when Scenario =>
            Handler.Inside_Scenario := False;
            Handler.Current_Project.Scenarios.Insert
              (New_Item => Handler.Scenario);
            --  Exit scenario

            --  Reset Handler scenario
            Handler.Scenario    := Scenarios.Null_Scenario;
         when Action =>
            if not Handler.Inside_Scenario then
               --  Append this action to actions map
               Handler.Current_Project.Actions.Insert
                 (New_Item    => Handler.Action);

               --  Reset Handler Action
               Handler.Action := Actions.Null_Action;
            else
               --  Append this action to scenario actions vector
               Handler.Scenario.Actions.Append (Handler.Ref_Action);

               Handler.Ref_Action := Actions.Null_Ref_Action;
            end if;
         when SCM_Action =>
            --  Append this action to scenario actions vector

            Handler.Scenario.Actions.Append (Handler.Ref_Action);

            Handler.Ref_Action := Actions.Null_Ref_Action;
         when Cmd =>
            Handler.Action.Cmd := Actions.Command (Handler.Content_Value);
         when SCM | Project | Name =>
            null;
      end case;

      Handler.Content_Value := Null_Unbounded_String;

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

   function Parse (Filename : String) return Project_Config is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      if not Directories.Exists (Name => Filename) then
         raise Config_Error with "No Project at path :" & Filename;
      end if;

      Input_Sources.File.Open
        (Filename => Filename,
         Input    => Source);

      Parse (Reader, Source);

      Input_Sources.File.Close (Source);
      return Reader.Current_Project;
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

      --  Set global state

      case NV is
         when Scenario =>
            Handler.Inside_Scenario := True;
         when SCM_Action =>
            --  SCM Action should be only inside scenari

            if not Handler.Inside_Scenario then
               raise Config_Error with "SCM Action outside scenario";
            end if;
         when others => null;
      end case;

      for J in 0 .. Get_Length (Atts) - 1 loop
         Attr := Get_Attribute (Get_Qname (Atts, J));
         if not XML_Schema (NV) (Attr) then
            raise Config_Error with "Unknow attribute "
            & Node_Value'Image (NV) & "." & Get_Qname (Atts, J);
         elsif Attr = Id then
            case NV is
               when Scenario =>
                  Handler.Scenario.Id :=
                    Scenarios.Id_Utils.Value (Get_Value (Atts, J));

               when SCM =>
                  Handler.Current_Project.SCM_Id :=
                    Savadur.SCM.Id_Utils.Value (Get_Value (Atts, J));

               when Variable =>
                  Handler.Var.Name :=
                    Savadur.Variables.Name_Utils.Value (Get_Value (Atts, J));

               when Action | SCM_Action =>
                  if NV = Action and then not Handler.Inside_Scenario then
                     Handler.Action.Id :=
                       Savadur.Actions.Id_Utils.Value (Get_Value (Atts, J));
                  else
                     Handler.Ref_Action.Id :=
                       Savadur.Actions.Id_Utils.Value (Get_Value (Atts, J));

                     --  Set the ref action type

                     if NV = Action then
                        Handler.Ref_Action.Action_Type := Actions.Default;
                     else
                        Handler.Ref_Action.Action_Type := Actions.SCM;
                     end if;
                  end if;

               when Name =>
                  Handler.Current_Project.Project_Id :=
                    Project_Id (+Get_Value (Atts, J));

               when others => null;
            end case;
         elsif Attr = Value then
            case NV is
               when Variable =>
                  Handler.Var.Value := +Get_Value (Atts, J);

               when Action | SCM_Action =>
                  if not Handler.Inside_Scenario then
                     raise Config_Error with "Unknow attribute "
                       & Node_Value'Image (NV) & "." & Get_Qname (Atts, J);
                  end if;
                  Handler.Ref_Action.Value := +Get_Value (Atts, J);

               when others => null;
            end case;
         elsif Attr = Result then
            case NV is
               when Action | SCM_Action =>
                  if Handler.Inside_Scenario then
                     raise Config_Error with "Unknow attribute "
                       & Node_Value'Image (NV) & "." & Get_Qname (Atts, J);
                  end if;
                  Get_Action_Result_Type : begin
                     Handler.Action.Result :=
                       Actions.Result_Type'Value (Get_Value (Atts, J));
                  exception
                     when Constraint_Error =>
                        raise Config_Error with "Unknow attribute value "
                          & Node_Value'Image (NV) & "." & Get_Qname (Atts, J)
                          & " value = " & Get_Value (Atts, J);
                  end Get_Action_Result_Type;

               when others => null;
            end case;
         else
            raise Config_Error with "Internal error for "
              & Node_Value'Image (NV) & " with " & Get_Qname (Atts, J);
         end if;
      end loop;
   end Start_Element;

end Savadur.Config.Project;
