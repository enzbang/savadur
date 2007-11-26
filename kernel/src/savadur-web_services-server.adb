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

with Savadur.Jobs.Client;
with Savadur.Signed_Files;

package body Savadur.Web_Services.Server is

   ---------
   -- Run --
   ---------

   procedure Run
     (Scenario       : in String;
      Signed_Project : in Server.Signed_Project) is
   begin
      --  This is run on the client side
      Jobs.Client.Queue.Add
        (Project  => Signed_Files.To_Handler
           (Signed_Files.External_Handler (Signed_Project)),
         Scenario => Scenario);
   end Run;

end Savadur.Web_Services.Server;
