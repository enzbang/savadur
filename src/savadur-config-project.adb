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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

with Savadur.Utils;
with Savadur.Variables;
with Savadur.Scenarios;
with Savadur.Actions;
with Savadur.SCM;

package body Savadur.Config.Project is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   type Node_Value is
     (SCM, Variable, SCM_Action,
      Action, Scenario, Cmd,
      Notifications, On_Failure, On_Success,
      Project, Name);

   type Attribute is (Id, Value, Result, Require_Change, On_Error);

   type XML_Attribute is array (Attribute) of Boolean;

   type XML_Schema is array (Node_Value) of XML_Attribute;

   Schema : constant XML_Schema
     := XML_Schema'(SCM           =>  XML_Attribute'(Id     => True,
                                                     others => False),
                    Variable      =>  XML_Attribute'(Id     => True,
                                                     Value  => True,
                                                     others => False),
                    SCM_Action    => XML_Attribute'(Id             => True,
                                                    Value          => True,
                                                    Require_Change => True,
                                                    On_Error       => True,
                                                    others         => False),
                    Action        => XML_Attribute'(Id             => True,
                                                    Value          => True,
                                                    Require_Change => True,
                                                    On_Error       => True,
                                                    others         => False),
                    Scenario      => XML_Attribute'(Id     => True,
                                                    others => False),
                    Cmd           => XML_Attribute'(others => False),
                    Project       => XML_Attribute'(others => False),
                    Notifications => XML_Attribute'(others => False),
                    On_Failure    => XML_Attribute'(others => False),
                    On_Success    => XML_Attribute'(others => False),
                    Name          => XML_Attribute'(Id     => True,
                                                    others => False));

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attributed value matching the given string or raise
   --  Config_Error

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Content_Value        : Unbounded_String;
      Var                  : Variables.Variable;
      Action               : Actions.Action;
      Ref_Action           : Actions.Ref_Action;
      Scenario             : Scenarios.Scenario;
      Inside_Scenario      : Boolean := False;
      Inside_Notifications : Boolean := False;
      Current_Project      : aliased Projects.Project_Config;
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

         when Notifications =>
            Handler.Inside_Notifications := False;

         when On_Failure =>
            Handler.Current_Project.Notifications.On_Failure.Append
              (New_Item => Handler.Ref_Action);

         when On_Success =>
            Handler.Current_Project.Notifications.On_Success.Append
              (New_Item => Handler.Ref_Action);

         when Action =>
            if not Handler.Inside_Notifications then
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

   function Get_Attribute (S : in String) return Attribute is
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

   function Parse
     (Project_Name : in String;
      Filename     : in String := "") return Projects.Project_Config
   is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      --  Set the project name

      Reader.Current_Project.Project_Id :=
        Projects.Id_Utils.Value (Project_Name);

      if Filename /= "" then
         Projects.Set_Filename (Reader.Current_Project'Access, Filename);
      end if;

      if not Directories.Exists
        (Projects.Project_Filename (Reader.Current_Project'Access))
      then
         raise Config_Error with "No Project at path :"
           & Projects.Project_Filename (Reader.Current_Project'Access);
      end if;

      --  Get default variable;

      Savadur.Variables.Default (Reader.Current_Project'Access);

      Input_Sources.File.Open
        (Filename => Projects.Project_Filename
           (Reader.Current_Project'Access),
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

      procedure Get_Attribute_Value (Position : in Natural);
      --  Get attribute value

      -------------------------
      -- Get_Attribute_Value --
      -------------------------

      procedure Get_Attribute_Value (Position : in Natural) is
      begin
         case Attr is
            when Id =>
               case NV is
                  when Scenario =>
                     Handler.Scenario.Id :=
                       Scenarios.Id_Utils.Value (Get_Value (Atts, Position));

                  when SCM =>
                     Handler.Current_Project.SCM_Id :=
                       Savadur.SCM.Id_Utils.Value (Get_Value (Atts, Position));

                  when Variable =>
                     Handler.Var.Name :=
                       Variables.Name_Utils.Value (Get_Value (Atts, Position));

                  when Action | SCM_Action =>
                     if NV = Action
                       and then not Handler.Inside_Scenario
                       and then not Handler.Inside_Notifications
                     then
                        Handler.Action.Id :=
                          Actions.Id_Utils.Value (Get_Value (Atts, Position));
                     else
                        Handler.Ref_Action.Id :=
                          Actions.Id_Utils.Value (Get_Value (Atts, Position));

                        --  Set the ref action type

                        if NV = Action then
                           Handler.Ref_Action.Action_Type := Actions.Default;
                        else
                           Handler.Ref_Action.Action_Type := Actions.SCM;
                        end if;
                     end if;

                  when Name =>
                     Handler.Current_Project.Project_Id :=
                       Projects.Project_Id (+Get_Value (Atts, Position));

                  when others => null;
               end case;

            when Value =>
               case NV is
                  when Variable =>
                     Handler.Var.Value := +Get_Value (Atts, Position);

                  when Action | SCM_Action =>
                     if not Handler.Inside_Scenario then
                        raise Config_Error with "Unknow attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;
                     Handler.Ref_Action.Value := +Get_Value (Atts, Position);

                  when others => null;
               end case;

            when Result =>
               case NV is
                  when Action | SCM_Action =>
                     if Handler.Inside_Scenario then
                        raise Config_Error with "Unknow attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;
                     Get_Action_Result_Type : begin
                        Handler.Action.Result :=
                          Actions.Result_Type'Value
                            (Get_Value (Atts, Position));
                     exception
                        when Constraint_Error =>
                           raise Config_Error with "Unknow attribute value "
                             & Node_Value'Image (NV) & "."
                             & Get_Qname (Atts, Position)
                             & " value = " & Get_Value (Atts, Position);
                     end Get_Action_Result_Type;

                  when others => null;
               end case;

            when Require_Change =>
               case NV is
                  when Action | SCM_Action =>
                     if not Handler.Inside_Scenario then
                        raise Config_Error with "Unknow attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;

                     Get_Require_Change_Value : begin
                        Handler.Ref_Action.Require_Change :=
                          Boolean'Value (Get_Value (Atts, Position));
                     exception
                        when Constraint_Error =>
                           raise Config_Error with "Unknow attribute value "
                             & Node_Value'Image (NV) & "."
                             & Get_Qname (Atts, Position)
                             & " value = " & Get_Value (Atts, Position);
                     end Get_Require_Change_Value;

                  when others => null;
               end case;

            when On_Error =>
               case NV is
                  when Action | SCM_Action =>
                     if not Handler.Inside_Scenario then
                        raise Config_Error with "Unknow attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;

                     Get_On_Error_Value : begin
                        Handler.Ref_Action.On_Error :=
                          Actions.On_Error_Hook'Value
                            (Get_Value (Atts, Position));
                     exception
                        when Constraint_Error =>
                           raise Config_Error with "Unknow attribute value "
                             & Node_Value'Image (NV) & "."
                             & Get_Qname (Atts, Position)
                             & " value = " & Get_Value (Atts, Position);
                     end Get_On_Error_Value;

                  when others => null;
               end case;
         end case;
      end Get_Attribute_Value;

   begin
      --  Set global state

      case NV is
         when Scenario =>
            Handler.Inside_Scenario := True;

         when Notifications =>
            Handler.Inside_Notifications := True;

         when SCM_Action =>
            --  SCM Action should be only inside scenari

            if not Handler.Inside_Scenario then
               raise Config_Error with "SCM Action outside scenario";
            end if;
         when others => null;
      end case;

      for J in 0 .. Get_Length (Atts) - 1 loop
         Attr := Get_Attribute (Get_Qname (Atts, J));

         --  Check if attribute is valid

         if not Schema (NV) (Attr) then
            raise Config_Error with "Unknow attribute "
              & Node_Value'Image (NV) & "." & Get_Qname (Atts, J);
         end if;

         Get_Attribute_Value (J);
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Exception_Information (E));
   end Start_Element;

end Savadur.Config.Project;
