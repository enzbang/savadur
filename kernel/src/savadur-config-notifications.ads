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

with AWS.Jabber;

package Savadur.Config.Notifications is
   Config_Error : exception renames Savadur.Config.Config_Error;

   procedure Parse;
   --  Parses all projects

   function Jabber_Server return String;
   --  Returns configured jabber server

   function Jabber_JID return String;
   --  Returns configured jabber jid

   function Jabber_Password return String;
   --  Returns configured jabber password

   function Jabber_Auth_Type return AWS.Jabber.Authentication_Type;
   --  Returns configured jabber auth_type
end Savadur.Config.Notifications;
