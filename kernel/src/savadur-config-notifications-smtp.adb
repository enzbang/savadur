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
with Unicode.CES;

with Savadur.Logs;
with Savadur.Utils;

package body Savadur.Config.Notifications.SMTP is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur.Utils;

   type SMTP_Config is record
      Server    : Unbounded_String;
      User      : Unbounded_String;
      Password  : Unbounded_String;
   end record;

   SMTP_Conf : SMTP_Config :=
                 (+"localhost", Null_Unbounded_String, Null_Unbounded_String);
   --  Default using localhost as mail server

   type Node_Value is (SMTP, Server, User, Password);

   type Tree_Reader is new Sax.Readers.Reader with record
      SMTP_Node     : Boolean;
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
            SMTP_Conf.Server := Handler.Content_Value;

         when User =>
            SMTP_Conf.User := Handler.Content_Value;

         when Password =>
            SMTP_Conf.Password := Handler.Content_Value;

         when SMTP =>
            Handler.SMTP_Node := False;
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
                        Name                 => "notify_smtp.xml");

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

   --------------
   -- Password --
   --------------

   function Password return String is
   begin
      return -SMTP_Conf.Password;
   end Password;

   ------------
   -- Server --
   ------------

   function Server return String is
   begin
      return -SMTP_Conf.Server;
   end Server;

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
         when SMTP =>
            Handler.SMTP_Node := True;

         when Server | User | Password =>
            --  Always take the first attribute value as there is only one
            --  attribute.
            Handler.Content_Value := +Get_Value (Atts, 0);
      end case;
   end Start_Element;

   ----------
   -- User --
   ----------

   function User return String is
   begin
      return -SMTP_Conf.User;
   end User;

end Savadur.Config.Notifications.SMTP;
