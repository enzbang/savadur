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

with Ada.Strings.Unbounded;

with SOAP.Utils;

package Savadur.Web_Services.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   type Metadata is record
      OS : Unbounded_String;
   end record;

   No_Metadata : constant Metadata;

   procedure Register
     (Key               : in String;
      Data              : in Metadata;
      Server_Name       : in String;
      Callback_Endpoint : in String);
   --  Registers a new client whose id is Key and with the given metadata. The
   --  endpoint is the SOAP callback for the server to reach the client.

   type Signed_Project is new String;

   type File_Data is record
      Filename : Unbounded_String;
      Content  : Unbounded_String;
   end record;

   No_File : constant File_Data;

   function Load_Project
     (Signed_Project : in Client.Signed_Project) return File_Data;
   --  Returns the project content from a server

   type Signed_SCM is new String;

   function Load_SCM (Signed_SCM : in Client.Signed_SCM) return File_Data;
   --  Returns the project content from a server

   type Name_Set is array (Positive range <>) of Unbounded_String;
   type Name_Set_Access is access Name_Set;

   package Name_Set_Safe_Pointer is
     new SOAP.Utils.Safe_Pointers (Name_Set, Name_Set_Access);

   type Diff_Data is record
      V1, V2     : Unbounded_String;
      Committers : Name_Set_Safe_Pointer.Safe_Pointer;
   end record;

   No_Diff_Data : constant Diff_Data;

   procedure Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Log_Filename : in String;
      Output       : in String;
      Result       : in Boolean;
      Job_Id       : in Natural;
      Diff_Data    : in Client.Diff_Data);
   --  Status is called by the client to register status of each action in the
   --  given scenario.

   function Ping return String;
   --  Returns pong if server is alive

private

   No_File      : constant File_Data :=
                    File_Data'(Filename => Null_Unbounded_String,
                               Content  => Null_Unbounded_String);

   No_Metadata  : constant Metadata :=
                    Metadata'(OS => Null_Unbounded_String);

   No_Diff_Data : constant Diff_Data :=
                    Diff_Data'(Null_Unbounded_String,
                               Null_Unbounded_String,
                               Name_Set_Safe_Pointer.To_Safe_Pointer
                            (Name_Set'(1 .. 0 => Null_Unbounded_String)));

end Savadur.Web_Services.Client;
