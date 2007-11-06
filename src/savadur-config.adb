
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;

with Input_Sources.File;
with Unicode.CES;

with Savadur.Scenario;
with Savadur.Action;

package body Savadur.Config is

   use Ada;
   use Ada.Strings.Unbounded;

   Config_Error : exception;

   type Node_Value is (SCM, SCM_Action, Action, Scenario, Cmd, Project);

   subtype Action_Node_Value is Node_Value range SCM_Action .. Action;

   type Attribute is (Id, Mode);

   subtype SCM_Attribute is Attribute range Id .. Id;
   subtype Action_Attribute is Attribute range Id .. Id;
   subtype Scenario_Attribute is Attribute;

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
      NV : Node_Value := Get_Node_Value (Local_Name);
   begin

      Text_IO.Put_Line (Local_Name);

      case NV is
         when Scenario =>
            Text_IO.Put_Line ("Scenario found : "
                              & To_String (Handler.Scenario_Id)
                              & " => "
                              & To_String
                                (Unbounded_String (Handler.Scenario.Mode))
                              & Savadur.Action.Image (Handler.Scenario.Actions));
            Handler.Inside_Scenario := False;
            Handler.Current_Project.Scenari.Insert
              (Key      => Savadur.Scenario.Id
                 (To_String (Handler.Scenario_Id)),
               New_Item => Handler.Scenario);
            --  Exit scenario
         when Action =>
            if not Handler.Inside_Scenario then
               --  Append this action to actions map
               Handler.Current_Project.Actions.Insert
                 (Key      => Savadur.Action.Id
                    (To_String (Handler.Action_Id)),
                  New_Item => Handler.Action);
            end if;
         when SCM_Action =>
            --  Append this action to scenario actions vector
            Handler.Scenario.Actions.Append
              (Savadur.Action.Id (To_String (Handler.Action_Id)));
         when Cmd =>
            Handler.Action.Cmd := Savadur.Action.Command (Handler.Value);
         when SCM | Project =>
            null;
      end case;

      Handler.Value := Null_Unbounded_String;

   end End_Element;

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

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (S : String) return Attribute is
      Upper_S : String := S;
      use GNAT;
   begin
      Case_Util.To_Upper (Upper_S);

      for SA in Scenario_Attribute'Range loop
         if Scenario_Attribute'Image (SA) = Upper_S then
            return SA;
         end if;
      end loop;

      raise Config_Error with "Unknown node " & S;
   end Get_Attribute;

   -----------
   -- Parse --
   -----------

   function Parse (Filename : String) return Project_Config is
      Reader : Tree_Reader;
      Source : Input_Sources.File.File_Input;
   begin
      Reader.Current_Project :=
        Project_Config'(SCM     => Null_Unbounded_String,
                        Actions => Savadur.Action.Maps.Empty_Map,
                        Scenari => Savadur.Scenario.Maps.Empty_Map);

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
      NV   : Node_Value := Get_Node_Value (Local_Name);

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
                     Handler.Scenario_Id :=
                       To_Unbounded_String (Get_Value (Atts, J));
                  when Mode =>
                     Handler.Scenario.Mode :=
                       Savadur.Scenario.Mode
                         (To_Unbounded_String (Get_Value (Atts, J)));
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
                     Handler.Current_Project.SCM :=
                       To_Unbounded_String (Get_Value (Atts, J));
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
                     Handler.Action_Id :=
                       To_Unbounded_String (Get_Value (Atts, J));
               end case;
            end loop;
         when Action =>

            if Handler.Inside_Scenario then
               for J in 0 .. Get_Length (Atts) - 1 loop
                  Attr := Get_Attribute (Get_Qname (Atts, J));
                  if Attr not in Action_Attribute then
                     raise Config_Error with " Unknow action attribute "
                       & Get_Qname (Atts, J);
                  end if;
                  case Action_Attribute (Attr) is
                     when Id =>
                        Handler.Action_Id :=
                          To_Unbounded_String (Get_Value (Atts, J));
                  end case;
               end loop;
            end if;
         when Cmd | Project =>
            null;
      end case;

   end Start_Element;

end Savadur.Config;
