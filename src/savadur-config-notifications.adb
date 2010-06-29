------------------------------------------------------------------------------
--                                 Savadur                                  --
--                                                                          --
--                         Copyright (C) 2008-2010                          --
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

with Input_Sources.File;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;

with Savadur.Logs;
with Savadur.Utils;

package body Savadur.Config.Notifications is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type Node_Value is
     (Notifications, Jabber, N_SMTP,
      Server, JID, Password, Auth_Type, User, Sender);

   subtype Root_Node is Node_Value range Notifications .. N_SMTP;

   type Tree_Reader is new Sax.Readers.Reader with record
      Conf_Node     : Root_Node;
      Content_Value : Unbounded_String;
   end record;

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

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

   --  XMPP

   type XMPP_Config_Data is record
      Server    : Unbounded_String;
      JID       : Unbounded_String;
      Password  : Unbounded_String;
      Auth_Type : AWS.Jabber.Client.Authentication_Mechanism;
   end record;

   XMPP_Config : XMPP_Config_Data;

   package body XMPP is

      ---------------
      -- Auth_Type --
      ---------------

      function Auth_Type return AWS.Jabber.Client.Authentication_Mechanism is
      begin
         return XMPP_Config.Auth_Type;
      end Auth_Type;

      ---------
      -- JID --
      ---------

      function JID return String is
      begin
         return -XMPP_Config.JID;
      end JID;

      --------------
      -- Password --
      --------------

      function Password return String is
      begin
         return -XMPP_Config.Password;
      end Password;

      ------------
      -- Server --
      ------------

      function Server return String is
      begin
         return -XMPP_Config.Server;
      end Server;

   end XMPP;

   --  SMTP

   type SMTP_Config_Data is record
      Server   : Unbounded_String;
      User     : Unbounded_String;
      Password : Unbounded_String;
      Sender   : Unbounded_String;
   end record;

   SMTP_Config : SMTP_Config_Data :=
                   SMTP_Config_Data'(+"localhost",
                                     Null_Unbounded_String,
                                     Null_Unbounded_String,
                                     Null_Unbounded_String);
   --  Default using localhost as mail server

   package body SMTP is

      --------------
      -- Password --
      --------------

      function Password return String is
      begin
         return -SMTP_Config.Password;
      end Password;

      ------------
      -- Sender --
      ------------

      function Sender return String  is
      begin
         return -SMTP_Config.Sender;
      end Sender;

      ------------
      -- Server --
      ------------

      function Server return String is
      begin
         return -SMTP_Config.Server;
      end Server;

      ----------
      -- User --
      ----------

      function User return String is
      begin
         return -SMTP_Config.User;
      end User;

   end SMTP;

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
         when Server =>
            case Handler.Conf_Node is
               when Notifications =>
                  raise Config_Error with "unexpected server node";

               when Jabber =>
                  XMPP_Config.Server := Handler.Content_Value;

               when N_SMTP =>
                  SMTP_Config.Server := Handler.Content_Value;
            end case;

         when JID =>
            XMPP_Config.JID := Handler.Content_Value;

         when Password =>
            case Handler.Conf_Node is
               when Notifications =>
                  raise Config_Error with "unexpected server node";

               when Jabber =>
                  XMPP_Config.Password := Handler.Content_Value;

               when N_SMTP =>
                  SMTP_Config.Password := Handler.Content_Value;
            end case;

         when Auth_Type =>
            XMPP_Config.Auth_Type :=
              AWS.Jabber.Client.
                Authentication_Mechanism'Value (-Handler.Content_Value);

         when User =>
            SMTP_Config.User := Handler.Content_Value;

         when Sender =>
            SMTP_Config.Sender := Handler.Content_Value;

         when Jabber | N_SMTP | Notifications =>
            Handler.Conf_Node := Notifications;
      end case;
   end End_Element;

   --------------------
   -- Get_Node_Value --
   --------------------

   function Get_Node_Value (S : in String) return Node_Value is
      Upper_S : constant String := Ada.Characters.Handling.To_Upper (S);

      function Image (N : in Node_Value) return String;
      --  Return N image, handling special case N_SMTP which is SMTP

      -----------
      -- Image --
      -----------

      function Image (N : in Node_Value) return String is
         I : constant String := Node_Value'Image (N);
      begin
         if I = "N_SMTP" then
            return "SMTP";
         else
            return I;
         end if;
      end Image;

   begin
      for NV in Node_Value'Range loop
         if Image (NV) = Upper_S then
            return NV;
         end if;
      end loop;

      raise Config_Error with "Unknown node " & S;
   end Get_Node_Value;

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
           (Content => "Can not parse " & Filename & " : file does no exist",
            Kind    => Logs.Handler.Warnings);
      else
         Input_Sources.File.Open (Filename => Filename, Input => Source);
         Parse (Reader, Source);
         Input_Sources.File.Close (Source);
      end if;
   end Parse;

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
      NV : constant Node_Value := Get_Node_Value (Local_Name);

   begin
      case NV is
         when Notifications =>
            Handler.Conf_Node := Notifications;

         when Jabber =>
            Handler.Conf_Node := Jabber;

         when N_SMTP =>
            Handler.Conf_Node := N_SMTP;

         when Server | JID | Password | Auth_Type | User | Sender =>
            --  Always take the first attribute value as there is only one
            --  attribute.
            Handler.Content_Value := +Get_Value (Atts, 0);
      end case;
   end Start_Element;

end Savadur.Config.Notifications;
