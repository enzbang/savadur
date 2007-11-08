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

with GNAT.Case_Util;

with Savadur.Utils;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

package body Savadur.Config.Project is

   use Savadur.Utils;

   Config_Error : exception;

   type Node_Value is
     (SCM, Variable, SCM_Action, Action, Scenario, Cmd, Project, Name);

   type Attribute is (Id, Mode);

   subtype Variable_Attribute is Attribute range Id .. Id;
   subtype SCM_Attribute      is Attribute range Id .. Id;
   subtype Action_Attribute   is Attribute range Id .. Id;
   subtype Name_Attribute is Attribute range Id .. Id;
   subtype Scenario_Attribute is Attribute;

   function Get_Node_Value (S : String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : String) return Attribute;
   --  Returns the attributed value matching the given string or raise
   --  Config_Error

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      Value           : Unbounded_String;
      Id              : Unbounded_String;
      Action          : Actions.Action;
      Scenario        : Scenarios.Scenario;
      Scenario_Id     : Unbounded_String;
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
         when Variable =>
            --  Overwrite default values

            Handler.Current_Project.Variables.Include
              (Key      => -Handler.Id,
               New_Item => -Handler.Value);
         when Scenario =>
            Handler.Inside_Scenario := False;
            Handler.Current_Project.Scenarios.Insert
              (Key      => Savadur.Scenarios.Id (-Handler.Scenario_Id),
               New_Item => Handler.Scenario);
            --  Exit scenario
         when Action =>
            if -Handler.Id = "" then
               raise Config_Error with " Null action id !";
            end if;

            if not Handler.Inside_Scenario then
               --  Append this action to actions map
               Handler.Current_Project.Actions.Insert
                 (Key      => Actions.Id (-Handler.Id),
                  New_Item => Handler.Action);
            else
               --  Append this action to scenario actions vector
               Handler.Scenario.Actions.Append
                 (Actions.Ref_Action'
                    (Action_Type => Actions.Default,
                     Id          => Actions.U_Id (Handler.Id)));

            end if;
         when SCM_Action =>
            --  Append this action to scenario actions vector
            Handler.Scenario.Actions.Append
              (Actions.Ref_Action'
                 (Action_Type => Actions.SCM,
                  Id          => Actions.U_Id (Handler.Id)));

         when Cmd =>
            Handler.Action.Cmd := Actions.Command (Handler.Value);
         when SCM | Project | Name =>
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

   function Parse (Filename : String) return Project_Config is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
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
      case NV is
         when Scenario =>
            Handler.Inside_Scenario := True;
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               if Attr not in Scenario_Attribute then
                  raise Config_Error with " Unknow scenario attribute "
                    & Get_Qname (Atts, J);
               end if;
               case Scenario_Attribute (Attr) is
                  when Id =>
                     Handler.Scenario_Id := +Get_Value (Atts, J);
                  when Mode =>
                     Handler.Scenario.Mode :=
                       Scenarios.Mode (+Get_Value (Atts, J));
               end case;
            end loop;
         when SCM =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               if Attr not in SCM_Attribute then
                  raise Config_Error with " Unknow SCM attribute "
                    & Get_Qname (Atts, J);
               end if;
               case SCM_Attribute (Attr) is
                  when Id =>
                     Handler.Current_Project.SCM_Id :=
                       Savadur.SCM.U_Id (+Get_Value (Atts, J));
               end case;
            end loop;
         when Name =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               if Attr not in Name_Attribute then
                  raise Config_Error with " Unknow Name attribute "
                    & Get_Qname (Atts, J);
               end if;
               case Name_Attribute (Attr) is
                  when Id =>
                     Handler.Current_Project.Project_Id :=
                       Project_Id (+Get_Value (Atts, J));
               end case;
            end loop;
         when SCM_Action =>

            --  SCM Action should be only inside scenari

            if not Handler.Inside_Scenario then
               raise Config_Error with "SCM Action outside scenario";
            end if;

            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               if Attr not in Action_Attribute then
                  raise Config_Error with " Unknow action attribute "
                    & Get_Qname (Atts, J);
               end if;
               case Action_Attribute (Attr) is
                  when Id =>
                     Handler.Id := +Get_Value (Atts, J);
               end case;
            end loop;
         when Action =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               if Attr not in Action_Attribute then
                  raise Config_Error with " Unknow action attribute "
                    & Get_Qname (Atts, J);
               end if;
               case Action_Attribute (Attr) is
                  when Id =>
                     Handler.Id := +Get_Value (Atts, J);
               end case;
            end loop;
         when Variable =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               if Attr not in Variable_Attribute then
                  raise Config_Error with " Unknow action attribute "
                    & Get_Qname (Atts, J);
               end if;
               case Variable_Attribute (Attr) is
                  when Id =>
                     Handler.Id := +Get_Value (Atts, J);
               end case;
            end loop;
         when Cmd | Project =>
            null;
      end case;

   end Start_Element;

end Savadur.Config.Project;
