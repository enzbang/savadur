------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

with Savadur.Actions;
with Savadur.Config.Cmd;
with Savadur.Config.Filters;
with Savadur.Logs;
with Savadur.Scenarios;
with Savadur.SCM;
with Savadur.Signed_Files;
with Savadur.Times;
with Savadur.Utils;
with Savadur.Variables;

package body Savadur.Config.Project is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   type Node_Value is
     (SCM, Variable, SCM_Action,
      Action, Scenario, Cmd,
      Notifications, On_Failure, On_Success,
      Project, Name, Description, Filter);

   type Attribute is
     (Id, Value, Result, On_Error, Status, Periodic,
      Filter1, Filter2, Use_Tmp, Patch_File);

   type XML_Attribute is array (Attribute) of Boolean;

   type XML_Schema is array (Node_Value) of XML_Attribute;

   Schema : constant XML_Schema := XML_Schema'
     (SCM           => XML_Attribute'(Id         => True,    others => False),
      Variable      => XML_Attribute'(Id | Value => True,    others => False),
      SCM_Action    => XML_Attribute'
        (Id | Value | Status | On_Error | Filter1 | Filter2 => True,
         others                                             => False),
      Action        => XML_Attribute'
        (Id | Value | Status | On_Error => True,             others => False),
      Scenario      => XML_Attribute'
        (Id | Periodic | Use_Tmp | Patch_File => True,       others => False),
      Project       => XML_Attribute'(others => False),
      Description   => XML_Attribute'(others => False),
      Notifications => XML_Attribute'(others => False),
      On_Failure    => XML_Attribute'(others => False),
      On_Success    => XML_Attribute'(others => False),
      Name          => XML_Attribute'(Id     => True,        others => False),
      --  Following nodes are parsed externally (Config.Cmd and Config.Filter)
      Filter        => XML_Attribute'(others => False),
      Cmd           => XML_Attribute'(others => False));

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attributed value matching the given string or raise
   --  Config_Error.

   function Internal_Parse
     (Filename : in String) return Projects.Project_Config;
   --  Parses the given filename and return the parsed project

   --  SAX overloaded routines to parse the incoming XML stream

   type Tree_Reader is new Sax.Readers.Reader with record
      Content_Value        : Unbounded_String;
      Var                  : Variables.Variable;
      Action               : Actions.Action := Actions.Null_Action;
      Ref_Action           : Actions.Ref_Action;
      Scenario             : Scenarios.Scenario;
      Description          : Projects.Project_Description;
      Inside_Scenario      : Boolean := False;
      Inside_Notifications : Boolean := False;
      Current_Project      : aliased Projects.Project_Config;
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
                    (New_Item => Handler.Action);

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
            Config.Cmd.End_Element
              (Handler.Action.Cmd, To_String (Handler.Content_Value));

         when Description =>
            Handler.Current_Project.Description :=
              Projects.Project_Description (Handler.Content_Value);

         when SCM | Project | Name | Filter =>
            null;
      end case;

      Handler.Content_Value := Null_Unbounded_String;
   end End_Element;

   ---------
   -- Get --
   ---------

   function Get (Project_Name : in String) return Projects.Project_Config is
      use Projects.Id_Utils;
      C : Projects.Sets.Sets.Cursor;
   begin
      C := Projects.Sets.Keys.Find (Configurations, Project_Name);
      if Projects.Sets.Sets.Has_Element (C) then
         return Projects.Sets.Sets.Element (C);
      else
         Logs.Write (Content => "Try loading unknown project " & Project_Name,
                     Kind    => Logs.Handler.Error);
         raise Name_Error
           with "Try loading unknown project " & Project_Name;
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

   --------------------
   -- Internal_Parse --
   --------------------

   function Internal_Parse
     (Filename : in String) return Projects.Project_Config
   is
      use Projects.Id_Utils;
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      --  Set the project name

      Projects.Set_Filename (Reader.Current_Project'Access, Filename);

      if not Directories.Exists
        (Projects.Project_Filename (Reader.Current_Project'Access))
      then
         raise Config_Error with "No Project at path :"
           & Projects.Project_Filename (Reader.Current_Project'Access);
      end if;

      Input_Sources.File.Open
        (Filename => Projects.Project_Filename (Reader.Current_Project'Access),
         Input    => Source);

      Parse (Reader, Source);

      Input_Sources.File.Close (Source);

      --  Get default variable

      Savadur.Variables.Default (Reader.Current_Project'Access);

      --  Now compute set the signed files

      Signed_Files.Create
        (Reader.Current_Project.Signature,
         Name     => -Reader.Current_Project.Project_Id,
         Filename => Filename);

      return Reader.Current_Project;
   end Internal_Parse;

   -----------------------
   --  Is_Project_Name  --
   -----------------------

   function Is_Project_Name (Project_Name : in String) return Boolean is
      use Projects.Id_Utils;
      C : Projects.Sets.Sets.Cursor;
   begin
      C := Projects.Sets.Keys.Find (Configurations, Project_Name);

      if Projects.Sets.Sets.Has_Element (C) then
         return True;
      else
         return False;
      end if;
   end Is_Project_Name;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      use Ada.Directories;
      Dir : constant String := Config.Project_File_Directory;
      S   : Search_Type;
      D   : Directory_Entry_Type;
   begin
      Start_Search
        (Search    => S,
         Directory => Dir,
         Pattern   => "*.xml",
         Filter    => Filter_Type'(Ordinary_File => True,
                                   Directory     => False,
                                   Special_File  => False));

      Walk_Directories : while More_Entries (S) loop
         Get_Next_Entry (S, D);
         Load_Config : declare
            Filename : constant String := Full_Name (D);
            Project  : Projects.Project_Config;
            pragma Unreferenced (Project);
         begin
            Logs.Write
              (Content => "Read Project config file : " & Filename,
               Kind    => Logs.Handler.Verbose);
            Project := Parse (Filename);
         exception
            when Config_Error =>
               Logs.Write
                 (Content => "Wrong project file : " & Filename,
                  Kind    => Logs.Handler.Error);
         end Load_Config;
      end loop Walk_Directories;
   exception
      when Name_Error =>
         raise Config_Error with " No Project Directory ? (" & Dir & ')';
   end Parse;

   function Parse (Filename : in String) return Projects.Project_Config is
      Project : constant Projects.Project_Config := Internal_Parse (Filename);
   begin
      Configurations.Insert (Project);
      return Project;
   end Parse;

   ------------
   -- Reload --
   ------------

   procedure Reload (Project_Name : in String; Filename : in String := "") is
   begin
      Try_Reload : declare
         Old_Project : aliased Projects.Project_Config := Get (Project_Name);
         Filename    : constant String :=
                         Projects.Project_Filename (Old_Project'Access);
         New_Project : constant Projects.Project_Config :=
                         Internal_Parse (Filename);
      begin
         Configurations.Include (New_Project);
      end Try_Reload;

   exception
      when Name_Error =>

         --  Project is new. Load the given filename

         if Filename = "" then
            raise Config_Error;
         end if;

         Load : declare
            New_Project : constant Projects.Project_Config :=
                            Internal_Parse (Filename);
         begin
            Configurations.Include (New_Project);
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

      NV : constant Node_Value := Get_Node_Value (Local_Name);

      procedure Get_Attribute_Value (Position : in Natural);
      --  Gets attribute value

      -------------------------
      -- Get_Attribute_Value --
      -------------------------

      procedure Get_Attribute_Value (Position : in Natural) is
         Attr : Attribute;
      begin
         Attr := Get_Attribute (Get_Qname (Atts, Position));

         --  Check if attribute is valid

         if not Schema (NV) (Attr) then
            raise Config_Error with "(Project) Unknown attribute "
              & Node_Value'Image (NV) & "." & Get_Qname (Atts, Position);
         end if;

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

                  when others =>
                     null;
               end case;

            when Value =>
               case NV is
                  when Variable =>
                     Handler.Var.Value := +Get_Value (Atts, Position);

                  when Action | SCM_Action =>
                     if not Handler.Inside_Scenario then
                        raise Config_Error with "(Project) Unknown attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;
                     Handler.Ref_Action.Value := +Get_Value (Atts, Position);

                  when others =>
                     null;
               end case;

            when Result =>
               case NV is
                  when Action | SCM_Action =>
                     if Handler.Inside_Scenario then
                        raise Config_Error with "(Project) Unknown attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;
                     Get_Action_Result_Type : begin
                        Handler.Action.Result :=
                          Actions.Result_Type'Value
                            (Get_Value (Atts, Position));
                     exception
                        when Constraint_Error =>
                           raise Config_Error
                             with "(Project) Unknown attribute value "
                               & Node_Value'Image (NV) & "."
                               & Get_Qname (Atts, Position)
                               & " value = " & Get_Value (Atts, Position);
                     end Get_Action_Result_Type;

                  when others =>
                     null;
               end case;

            when Status =>
               case NV is
                  when Action | SCM_Action =>
                     if not Handler.Inside_Scenario then
                        raise Config_Error
                          with "(Project) Unknown attribute "
                            & Node_Value'Image (NV) & "."
                            & Get_Qname (Atts, Position);
                     end if;

                     Get_Status_Value : declare
                        Value : constant String :=
                                  Ada.Characters.Handling.To_Lower
                                    (Get_Value (Atts, Position));
                     begin
                        if Value /= "require_change"
                          and then Strings.Fixed.Index (Value, "=") = 0
                        then
                           raise Config_Error
                             with "(Project) Unknown attribute value "
                               & Node_Value'Image (NV) & "."
                               & Get_Qname (Atts, Position)
                               & " value = " & Value;

                        else
                           Handler.Ref_Action.Status := +Value;
                        end if;
                     end Get_Status_Value;

                  when others =>
                     null;
               end case;

            when On_Error =>
               case NV is
                  when Action | SCM_Action =>
                     if not Handler.Inside_Scenario then
                        raise Config_Error with "(Project) Unknown attribute "
                          & Node_Value'Image (NV) & "."
                          & Get_Qname (Atts, Position);
                     end if;

                     Get_On_Error_Value : begin
                        Handler.Ref_Action.On_Error :=
                          Actions.On_Error_Hook'Value
                            (Get_Value (Atts, Position));
                     exception
                        when Constraint_Error =>
                           raise Config_Error
                             with "(Project) Unknown attribute value "
                               & Node_Value'Image (NV) & "."
                               & Get_Qname (Atts, Position)
                               & " value = " & Get_Value (Atts, Position);
                     end Get_On_Error_Value;

                  when others =>
                     null;
               end case;

            when Periodic =>
               case NV is
                  when Scenario =>
                     if Get_Value (Atts, Position) /= "" then
                        Handler.Scenario.Periodic :=
                          Savadur.Times.Create (Get_Value (Atts, Position));
                     end if;

                  when others =>
                     null;
               end case;

            when Filter1 =>
               case NV is
                  when SCM_Action =>
                     Handler.Ref_Action.Filters (1) :=
                       Filters.Get_Id
                         (Projects.Id_Utils.To_String
                              (Handler.Current_Project.Project_Id),
                          Get_Value (Atts, Position));

                  when others =>
                     null;
               end case;

            when Filter2 =>
               case NV is
                  when SCM_Action =>
                     Handler.Ref_Action.Filters (2) :=
                       Filters.Get_Id
                         (Projects.Id_Utils.To_String
                              (Handler.Current_Project.Project_Id),
                          Get_Value (Atts, Position));

                  when others =>
                     null;
               end case;

            when Use_Tmp =>
               case NV is
                  when Scenario =>
                        Handler.Scenario.Use_Tmp := True;

                  when others =>
                     null;
               end case;

         when Patch_File =>
            case NV is
               when Scenario =>
                  Handler.Scenario.Patch_File := True;

               when others =>
                  null;
            end case;
         end case;

      exception
         when E : Constraint_Error =>
            Logs.Write
              (Content => Exception_Information (E),
               Kind    => Logs.Handler.Very_Verbose);
            raise Config_Error;
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

         when Cmd =>
            Config.Cmd.Start_Element
              (Command       => Handler.Action.Cmd,
               Prefix        => Projects.Id_Utils.To_String
                 (Handler.Current_Project.Project_Id),
               Namespace_URI => Namespace_URI,
               Local_Name    => Local_Name,
               Qname         => Qname,
               Atts          => Atts);
            return;

         when Filter =>
            Config.Filters.Start_Element
              (Prefix        => Projects.Id_Utils.To_String
                 (Handler.Current_Project.Project_Id),
               Namespace_URI => Namespace_URI,
               Local_Name    => Local_Name,
               Qname         => Qname,
               Atts          => Atts);
            return;

         when others =>
            null;
      end case;

      for J in 0 .. Get_Length (Atts) - 1 loop
         Get_Attribute_Value (J);
      end loop;
   end Start_Element;

end Savadur.Config.Project;
