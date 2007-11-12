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

with Ada.Strings.Unbounded;

package Savadur.SOAP.Client is

   use Ada.Strings.Unbounded;

   type Metadata is record
      OS : Unbounded_String;
   end record;

   procedure Register
     (Key               : in String;
      Data              : in Metadata;
      Callback_Endpoint : in String);
   --  Register a new client whose id is Key and with the given metadata. The
   --  endpoint is the SOAP callback for the server to reach the client.

   type Returned_Status is (Success, Failure);

   type Project is new String;

   procedure Status
     (Key      : in String;
      Ref      : in Project;
      Scenario : in String;
      Action   : in String;
      Result   : in String;
      Status   : in Returned_Status);
   --  Status is called by the client to register status of each action in the
   --  given scenario.

end Savadur.SOAP.Client;
