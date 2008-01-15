------------------------------------------------------------------------------
--                                 Savadur                                  --
--                                                                          --
--                            Copyright (C) 2008                            --
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

with GNAT.Case_Util;

with Input_Sources.File;

with Sax.Readers;
with Sax.Attributes;

with Savadur.Logs;
with Savadur.Utils;

with Unicode.CES;

package body Savadur.Config.Notifications is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type Jabber_Config is record
      Server    : Unbounded_String;
      JID       : Unbounded_String;
      Password  : Unbounded_String;
      Auth_Type : AWS.Jabber.Authentication_Type;
   end record;

   Jabber_Conf : Jabber_Config;

   type Node_Value is
     (Jabber, Server, JID, Password, Auth_Type);

   type Tree_Reader is new Sax.Readers.Reader with record
      Jabber_Node   : Boolean;
      Content_Value : Unbounded_String;
   end record;

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

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
         when Server =>
            Jabber_Conf.Server := Handler.Content_Value;

         when JID =>
            Jabber_Conf.JID := Handler.Content_Value;

         when Password =>
            Jabber_Conf.Password := Handler.Content_Value;

         when Auth_Type =>
            Jabber_Conf.Auth_Type :=
              AWS.Jabber.Authentication_Type'Value (-Handler.Content_Value);

         when Jabber =>
            Handler.Jabber_Node := False;
      end case;
   end End_Element;

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

   ----------------------
   -- Jabber_Auth_Type --
   ----------------------

   function Jabber_Auth_Type return AWS.Jabber.Authentication_Type is
   begin
      return Jabber_Conf.Auth_Type;
   end Jabber_Auth_Type;

   ----------------
   -- Jabber_JID --
   ----------------

   function Jabber_JID return String is
   begin
      return -Jabber_Conf.JID;
   end Jabber_JID;

   ---------------------
   -- Jabber_Password --
   ---------------------

   function Jabber_Password return String is
   begin
      return -Jabber_Conf.Password;
   end Jabber_Password;

   -------------------
   -- Jabber_Server --
   -------------------

   function Jabber_Server return String is
   begin
      return -Jabber_Conf.Server;
   end Jabber_Server;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      Config_Dir : constant String :=
                     Directories.Compose
                       (Containing_Directory => Config.Savadur_Directory,
                        Name                 => "config");
      Filename   : constant String :=
                     Directories.Compose
                       (Containing_Directory => Config_Dir,
                        Name                 => "notify.xml");

      Reader     : Tree_Reader;
      Source     : Input_Sources.File.File_Input;
   begin
      if not Directories.Exists (Filename) then
         Logs.Write
           (Content => "Can not parse notify.xml : file does no exist",
            Kind    => Logs.Handler.Warnings);
      end if;
      Input_Sources.File.Open (Filename => Filename, Input => Source);
      Parse (Reader, Source);
      Input_Sources.File.Close (Source);
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

   begin
      case NV is
         when Jabber =>
            Handler.Jabber_Node := True;

         when Server | JID | Password | Auth_Type =>
            --  Always take the first attribute value as there is only one
            --  attribute.
            Handler.Content_Value := +Get_Value (Atts, 0);
      end case;
   end Start_Element;

end Savadur.Config.Notifications;
