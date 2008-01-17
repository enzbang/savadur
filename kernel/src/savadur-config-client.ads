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

package Savadur.Config.Client is

   Config_Error : exception;

   function Get_Key return String;
   --  Returns client key

   function Get_Endpoint return String;
   --  Returns client endpoint

   function Get_Ping_Delay return Duration;
   --  Returns ping delay or 600.0 (10 minutes) if not specified

   function Get_Connection_Retry_Delay return Duration;
   --  Returns connection retry delay or 300.0  (5 minutes) if not specified

end Savadur.Config.Client;
