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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Unbounded;

package Savadur.Servers is

   use Ada;
   use Ada.Strings.Unbounded;

   type Online_Status is (Online, Offline);

   type Cursor is private;

   type Server is private;

   ----------------------------
   -- Online/Offline servers --
   ----------------------------

   function Get (Name : in String) return Server;
   --  Returns the server having the given name

   function URL (Server : in Servers.Server) return String;
   --  Returns server endpoint (or empty string is server is offline)

   function Log_Path (Server : in Servers.Server) return String;
   --  Returns server's log path

   function Log_Prefix (Server : in Servers.Server) return String;
   --  Returns server's log name prefix

   function Send_Log (Server : in Servers.Server) return Boolean;
   --  Returns True if the log should be sent to the server

   function Name (Position : in Cursor) return String;
   --  Returns server name

   function URL (Position : in Cursor) return String;
   --  Returns server url

   procedure Go_Offline (Server_Name : in Unbounded_String);
   --  Marks a server as offline

   procedure Go_Online (Server_Name : in Unbounded_String);
   --  Marks a server as online

   procedure Offline_Iterate
     (Process : not null access procedure (Position : in Cursor));
   --  Iterates on offline servers

   procedure Online_Iterate
     (Process : not null access procedure (Position : in Cursor));
   --  Iterates on offline servers

   function Image return String;
   --  Returns the servers set image

   function Length return Natural;
   --  Returns the servers set length

   procedure Insert
     (Name, URL, Log_Path, Log_Prefix : in String; Send_Log : in Boolean);
   --  Insert a new server

private

   type Server is record
      Name       : Unbounded_String;
      URL        : Unbounded_String;
      Log_Path   : Unbounded_String;
      Log_Prefix : Unbounded_String;
      Send_Log   : Boolean;
      Status     : Online_Status;
   end record;

   function Hash (Server : in Servers.Server) return Containers.Hash_Type;
   --  Renames Strings.Hash (on server name)

   function Key_Equal (S1, S2 : in Server) return Boolean;

   ----------
   -- Sets --
   ----------

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Server,
      Hash                => Hash,
      Equivalent_Elements => Key_Equal);

   function Image (Servers_Set : in Sets.Set) return String;
   --  Returns the Servers_Set image

   type Cursor is new Sets.Cursor;

   Empty_Server : constant Server :=
                    Server'(Name       => Null_Unbounded_String,
                            URL        => Null_Unbounded_String,
                            Log_Path   => Null_Unbounded_String,
                            Log_Prefix => Null_Unbounded_String,
                            Send_Log   => False,
                            Status     => Offline);
end Savadur.Servers;
