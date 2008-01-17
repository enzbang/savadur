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

with Ada.Directories;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;

with Sax.Readers;
with Sax.Attributes;
with Input_Sources.File;
with Unicode.CES;

with Savadur.Client_Service;
with Savadur.Server_Service;
with Savadur.Utils;

package body Savadur.Config.Client is

   use Ada;
   use Ada.Strings.Unbounded;
   use Savadur;
   use Savadur.Utils;

   type Config is record
      Client_Metadata        : Metadata;
      Connection_Retry_Delay : Duration;
      Description            : Unbounded_String;
      Endpoint               : Unbounded_String;
      Key                    : Unbounded_String;
      Ping_Delay             : Duration;
   end record;

   Empty : constant Config :=
             Config'(Key                    => Null_Unbounded_String,
                     Endpoint               => Null_Unbounded_String,
                     Ping_Delay             => 600.0,
                     Connection_Retry_Delay => 300.0);

   Configuration : Config := Empty;

   type Node_Value is (Client, Name, Endpoint,
                       Ping_Delay, Connection_Retry_Delay);

   type Attribute is (Id, URL, Seconds);

   function Get_Node_Value (S : in String) return Node_Value;
   --  Returns the node value matching the given string or raise Config_Error

   function Get_Attribute (S : in String) return Attribute;
   --  Returns the attribute value matching the given string or raise
   --  Config_Error.

   type Tree_Reader is new Sax.Readers.Reader with null record;

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure Parse;
   --  Parses client configuration file

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (S : in String) return Attribute is
      use GNAT;
      Upper_S : String := S;
   begin
      Case_Util.To_Upper (Upper_S);

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
      Filename : constant String := Ada.Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "client",
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
         if Savadur.Config.Client_Server or Savadur.Config.Is_Server then
            raise Config_Error with "No client.xml file !";
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

   procedure Start_Element
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
         when Client => null;

         when Name =>
            for J in 0 .. Get_Length (Atts) - 1 loop
               Attr := Get_Attribute (Get_Qname (Atts, J));
               case Attr is
                  when Id =>
                     Configuration.Key := +Get_Value (Atts, J);

                  when URL | Seconds =>
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

                  when Id | Seconds =>
                     raise Config_Error with "Wrong node attr "
                       & Attribute'Image (Attr);
               end case;
            end loop;

         when Ping_Delay =>
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

         when Connection_Retry_Delay =>
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
      end case;
   end Start_Element;

end Savadur.Config.Client;
