------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
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
with Ada.Text_IO;

with Savadur.Clients;
with Savadur.Config;
with Savadur.Logs;
with Savadur.Signed_Files;
with Savadur.Utils;

package body Savadur.Web_Services.Client is

   use Ada;
   use Savadur.Utils;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Project_Name : in String;
      SHA1         : in String) return String
   is
      use type Signed_Files.Signature;
      Project_Filename : constant String :=
                           Directories.Compose
                             (Config.Project_File_Directory,
                              Project_Name,
                              Extension => "xml");
      --  ??? Would be better to have the list of all projects in the server
      --  and be able to find the corresponding filename.
   begin
      Logs.Write ("Load_Project " & Project_Filename);
      if Directories.Exists (Project_Filename) then
         declare
            S_File : Signed_Files.Handler;
         begin
            Signed_Files.Create (S_File, Project_Filename);

            if SHA1'Length = 40
              and then
                Signed_Files.SHA1 (S_File) = Signed_Files.Signature (SHA1)
            then
               Logs.Write ("   up-to-date");
               --  Client up-to-date
               return "";

            else
               Logs.Write ("   content returned");
               return Utils.Content (Project_Filename);
            end if;
         end;

      else
         Logs.Write ("   project not found!");
         return "";
      end if;
   end Load_Project;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key               : in String;
      Data              : in Metadata;
      Callback_Endpoint : in String) is
   begin
      Text_IO.Put_Line
        ("Register new client : " & Key & '@' & Callback_Endpoint);
      Clients.Registered.Insert (New_Item => (+Key, Data, +Callback_Endpoint));
   end Register;

   ------------
   -- Status --
   ------------

   procedure Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Output       : in String;
      Result       : in Returned_Status)
   is
      pragma Unreferenced
        (Key, Project_Name, Scenario, Action, Output, Result);
   begin
      null;
   end Status;

end Savadur.Web_Services.Client;
