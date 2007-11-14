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

with Ada.Text_IO;

with Savadur.Clients;
with Savadur.Utils;

package body Savadur.Web_Services.Client is

   use Ada;
   use Savadur.Utils;

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
     (Key      : in String;
      Ref      : in Project;
      Scenario : in String;
      Action   : in String;
      Output   : in String;
      Result   : in Returned_Status)
   is
      pragma Unreferenced (Key, Ref, Scenario, Action, Output, Result);
   begin
      null;
   end Status;

end Savadur.Web_Services.Client;
