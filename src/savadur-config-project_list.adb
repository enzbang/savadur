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
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;

with Sax.Readers;
with Sax.Attributes;
with Input_Sources.File;
with Unicode.CES;

with Savadur.Utils;
with Savadur.Logs;

package body Savadur.Config.Project_List is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur;
   use Savadur.Utils;

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

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure Register_Client
     (Project  : in String;
      Scenario : in String;
      Client   : in String);
   --  Registers a new client which handle the given project/scenario

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
           (Containing_Directory => Config.Savadur_Directory, Name => "config"),
         Pattern   => "project_list.xml",
         Filter    => Filter_Type'(Ordinary_File => True,
                                   Directory     => False,
                                   Special_File  => False));

      Walk_Directories : while More_Entries (S) loop
         Get_Next_Entry (S, D);
         Load_Config : declare
            Filename : constant String := Full_Name (D);
         begin
            Logs.Write (Filename);

            Input_Sources.File.Open (Filename => Filename, Input => Source);
            Parse (Reader, Source);
            Input_Sources.File.Close (Source);
         end Load_Config;
      end loop Walk_Directories;
   exception
      when IO_Exceptions.Name_Error =>
         raise Config_Error with " No Servers Directory ?"
           & Directories.Compose
           (Containing_Directory => Config.Savadur_Directory, Name => "config");
   end Parse;

   ---------------------
   -- Register_Client --
   ---------------------

   procedure Register_Client
     (Project  : in String;
      Scenario : in String;
      Client   : in String)
   is
      procedure Update_Scenario
        (Key     : in     String;
         Element : in out Savadur.Project_List.Scenarios.Map);

      procedure Update_Client
        (Key     : in     String;
         Element : in out Savadur.Project_List.Clients.Vector);

      -------------------
      -- Update_Client --
      -------------------

      procedure Update_Client
        (Key     : in     String;
         Element : in out Savadur.Project_List.Clients.Vector)
      is
         pragma Unreferenced (Key);
      begin
         Savadur.Project_List.Clients.Append
           (Container => Element,
            New_Item  => Savadur.Project_List.Client'(+Client, True));
      end Update_Client;

      ---------------------
      -- Update_Scenario --
      ---------------------

      procedure Update_Scenario
        (Key     : in String;
         Element : in out Savadur.Project_List.Scenarios.Map)
      is
         pragma Unreferenced (Key);
         Position : Savadur.Project_List.Scenarios.Cursor :=
                      Element.Find (Scenario);
      begin
         if not Savadur.Project_List.Scenarios.Has_Element (Position) then
            Add_To_Scenario : declare
               V : Savadur.Project_List.Clients.Vector;
               I : Boolean;
            begin
               Element.Insert
                 (Scenario,
                  New_Item => V, Position => Position, Inserted => I);
            end Add_To_Scenario;
         end if;

         Element.Update_Element (Position, Update_Client'Access);
      end Update_Scenario;

      Position : Savadur.Project_List.Projects.Cursor :=
                   Configurations.Find (Project);

   begin
      if not Savadur.Project_List.Projects.Has_Element (Position) then
         Add_To_Project : declare
            M : Savadur.Project_List.Scenarios.Map;
            I : Boolean;
         begin
            Configurations.Insert
              (Project, New_Item => M, Position => Position, Inserted => I);
         end Add_To_Project;
      end if;

      Configurations.Update_Element (Position, Update_Scenario'Access);
   end Register_Client;

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
                     Register_Client
                       (Project  => -Handler.Project,
                        Scenario => -Handler.Scenario,
                        Client   => Get_Value (Atts, J));
                  when Id =>
                     raise Config_Error with "Unexpected Id attribute";
               end case;
            end loop;
      end case;
   end Start_Element;

end Savadur.Config.Project_List;
