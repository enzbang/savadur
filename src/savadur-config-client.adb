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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Templates;

with Sax.Readers;
with Sax.Attributes;
with Input_Sources.File;
with Unicode.CES;

with Savadur.Client_Service;
with Savadur.Config;
with Savadur.Logs;
with Savadur.Server_Service;
with Savadur.Utils;

package body Savadur.Config.Client is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;
   use Savadur;
   use Savadur.Utils;

   type Config is record
      Client_Metadata        : Web_Services.Client.Metadata;
      Connection_Retry_Delay : Duration;
      Description            : Unbounded_String;
      Endpoint               : Unbounded_String;
      Key                    : Unbounded_String;
      Ping_Delay             : Duration;
   end record;

   Empty : constant Config :=
             Config'(Client_Metadata        => Web_Services.Client.No_Metadata,
                     Connection_Retry_Delay => 300.0,
                     Description            => Null_Unbounded_String,
                     Endpoint               => Null_Unbounded_String,
                     Key                    => Null_Unbounded_String,
                     Ping_Delay             => 600.0);

   Configuration : Config := Empty;

   type Node_Value is (Server, Client, Connection_Retry_Delay,
                       Description,  Endpoint, Metadata,
                       Name, OS, Ping_Delay);

   type Attribute is (Id, URL, Seconds, Value);

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   type Tree_Reader is new Sax.Readers.Reader with null record;

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure Parse;
   --  Parses client configuration file

   function Config_Filename return String;
   --  Returns the base name (without extension) of the XML configuration file
   --  (either client or server).

   ---------------------
   -- Config_Filename --
   ---------------------

   function Config_Filename return String is
   begin
      if Savadur.Config.Is_Server then
         return "server";
      else
         return "client";
      end if;
   end Config_Filename;

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

   --------------------------------
   -- Get_Connection_Retry_Delay --
   --------------------------------

   function Get_Connection_Retry_Delay return Standard.Duration is
   begin
      if Configuration = Empty then
         Parse;
      end if;

      return Configuration.Connection_Retry_Delay;
   end Get_Connection_Retry_Delay;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description return String is
   begin
      if Configuration = Empty then
         Parse;
      end if;

      return -Configuration.Description;
   end Get_Description;

   --------------------
   --  Get_Endpoint  --
   --------------------

   function Get_Endpoint return String is
   begin
      if Configuration = Empty then
         Parse;
      end if;

      return -Configuration.Endpoint;
   end Get_Endpoint;

   -------------
   -- Get_Key --
   -------------

   function Get_Key return String is
   begin
      if Configuration = Empty then
         Parse;
      end if;

      return -Configuration.Key;
   end Get_Key;

   ------------------
   -- Get_Metadata --
   ------------------

   function Get_Metadata return Web_Services.Client.Metadata is
   begin
      if Savadur.Config.Is_Server then
         raise Config_Error with "Metadata are not defined for server";
      end if;

      if Configuration = Empty then
         Parse;
      end if;

      return Configuration.Client_Metadata;
   end Get_Metadata;

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
   -- Get_Ping_Delay --
   --------------------

   function Get_Ping_Delay return Standard.Duration is
   begin
      if Configuration = Empty then
         Parse;
      end if;

      return Configuration.Ping_Delay;
   end Get_Ping_Delay;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      Filename : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => Config_Filename,
         Extension            => "xml");
      Reader   : Tree_Reader;
      Source   : Input_Sources.File.File_Input;
   begin
      --  Set endpoint to the default value found in the WSDL

      if Savadur.Config.Is_Server then
         Configuration.Endpoint :=
           To_Unbounded_String (Savadur.Client_Service.URL);

      else
         Configuration.Endpoint :=
           To_Unbounded_String (Savadur.Server_Service.URL);
      end if;

      if not Directories.Exists (Filename) then
         if Savadur.Config.Client_Server or else Savadur.Config.Is_Server then
            raise Config_Error with "Cannot open " & Filename;
         else
            Configuration.Key := +"default";
         end if;

      else
         Input_Sources.File.Open
           (Filename => Filename,
            Input    => Source);

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
      pragma Unreferenced (Namespace_URI, Handler);
      pragma Unreferenced (Qname);

      use Sax.Attributes;
      NV   : constant Node_Value := Get_Node_Value (Local_Name);
      Attr : Attribute;

   begin
      case NV is
         when Server =>
            if not Savadur.Config.Is_Server then
               raise Config_Error with "Unexpected Server for client.";
            end if;

         when Client =>
            if Savadur.Config.Is_Server then
               raise Config_Error with "Unexpected Client for server.";
            end if;

         when Metadata =>
            null;

         when Connection_Retry_Delay =>
            if Savadur.Config.Is_Server then
               raise Config_Error with "Unexpected Retry_Delay for server.";

            else
               Connection_Retry_Delay_In_Seconds : begin
                  for J in 0 .. Get_Length (Atts) - 1 loop
                     Attr := Get_Attribute (Get_Qname (Atts, J));
                     if Attr = Seconds then
                        Configuration.Connection_Retry_Delay :=
                          Duration'Value (Get_Value (Atts, J));

                     else
                        raise Config_Error with "Wrong node attr "
                          & Attribute'Image (Attr);
                     end if;
                  end loop;

               exception
                  when Constraint_Error =>
                     raise Config_Error with "Wrong node value for attr "
                       & Attribute'Image (Attr);
               end Connection_Retry_Delay_In_Seconds;
            end if;

         when Description =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Value =>
                     Configuration.Description := +Get_Value (Atts, J);

                  when Id | URL | Seconds =>
                     raise Config_Error with "Wrong node attr "
                       & Attribute'Image (Attr);

               end case;
            end loop;

         when Endpoint =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when URL =>
                     Configuration.Endpoint := +Get_Value (Atts, J);

                  when Value | Id | Seconds =>
                     raise Config_Error with "Wrong node attr "
                       & Attribute'Image (Attr);
               end case;
            end loop;

         when Name =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Id =>
                     Configuration.Key := +Get_Value (Atts, J);

                  when Value | URL | Seconds =>
                     raise Config_Error with "Wrong node attr "
                       & Attribute'Image (Attr);

               end case;
            end loop;

         when OS =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Value =>
                     Configuration.Client_Metadata.OS := +Get_Value (Atts, J);

                  when Id | URL | Seconds =>
                     raise Config_Error with "Wrong node attr "
                       & Attribute'Image (Attr);

               end case;
            end loop;

         when Ping_Delay =>
            if Savadur.Config.Is_Server then
               raise Config_Error with "Unexpected Ping_Delay for server.";

            else
               Ping_Delay_In_Seconds : begin
                  for J in 0 .. Get_Length (Atts) - 1 loop
                     Attr := Get_Attribute (Get_Qname (Atts, J));
                     if Attr = Seconds then
                        Configuration.Ping_Delay :=
                          Duration'Value (Get_Value (Atts, J));

                     else
                        raise Config_Error with "Wrong node attr "
                          & Attribute'Image (Attr);
                     end if;
                  end loop;

               exception
                  when Constraint_Error =>
                     raise Config_Error with "Wrong node value for attr "
                       & Attribute'Image (Attr);
               end Ping_Delay_In_Seconds;
            end if;
      end case;
   end Start_Element;

   -----------
   -- Write --
   -----------

   procedure Write (Key, Endpoint, Description, OS : in String) is
      Filename  : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => Config_Filename,
         Extension            => "xml");
      Template  : constant String := Directories.Compose
        (Containing_Directory =>
           Savadur.Config.Config_Templates_Directory,
         Name                 => Config_Filename,
         Extension            => "txml");
      File      : Text_IO.File_Type;
      Set       : Templates.Translate_Set;
   begin
      if Directories.Exists (Filename) and then Configuration = Empty then
         Parse;
      end if;

      if Key /= "" then
         Configuration.Key := +Key;
      end if;

      if Endpoint /= "" then
         Configuration.Endpoint := +Endpoint;
      end if;

      if Description /= "" then
         Configuration.Description := +Description;
      end if;

      if OS /= "" then
        Configuration.Client_Metadata.OS := +OS;
      end if;

      Text_IO.Put_Line ("key is " & (-Configuration.Key));

      Templates.Insert
        (Set  => Set,
         Item => Templates.Assoc
           (Variable => "KEY",
            Value    => -Configuration.Key));

      Templates.Insert
        (Set  => Set,
         Item => Templates.Assoc
           (Variable => "ENDPOINT",
            Value    => -Configuration.Endpoint));

      Templates.Insert
        (Set  => Set,
         Item => Templates.Assoc
           (Variable => "DESCRIPTION",
            Value    => -Configuration.Description));

      Templates.Insert
        (Set  => Set,
         Item => Templates.Assoc
           (Variable => "METADATA_OS",
            Value    => -Configuration.Client_Metadata.OS));

      Set_Delays : declare
         Conn_Retry : constant String :=
                        Duration'Image
                          (Configuration.Connection_Retry_Delay);
         Ping       : constant String :=
                        Duration'Image (Configuration.Ping_Delay);
      begin
         if Conn_Retry /= "" then
            Templates.Insert
              (Set  => Set,
               Item => Templates.Assoc
                 (Variable => "CONNECTION_RETRY",
                  Value    => Conn_Retry
                    (Conn_Retry'First + 1 .. Conn_Retry'Last)));
         end if;

         if Ping /= "" then
            Templates.Insert
              (Set  => Set,
               Item => Templates.Assoc
                 (Variable => "PING",
                  Value    => Ping (Ping'First + 1 .. Ping'Last)));
         end if;
      end Set_Delays;

      if Directories.Exists (Template) then
         Text_IO.Create (File => File,
                         Mode => Text_IO.Out_File,
                         Name => Filename);
         Text_IO.Put (File,
           Templates.Parse
             (Filename     => Template,
              Translations => Set));
         Text_IO.Close (File);

      else
         Logs.Write (Content => "Missing template file: " & Template,
                     Kind    => Logs.Handler.Error);
      end if;
   end Write;

end Savadur.Config.Client;
