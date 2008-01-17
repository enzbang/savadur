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

with Savadur.Actions;

package Savadur.Notifications is

   type Hooks is record
      On_Success : Actions.Vectors.Vector;
      On_Failure : Actions.Vectors.Vector;
   end record;

   function Image (H : in Hooks) return String;
   --  Returns notification hooks image

   procedure XMPP_Send
     (JID          : in String;
      Subject      : in String;
      Content      : in String);
   --  Send jabber message to JID

   procedure Send_Mail
     (Email        : in String;
      Subject      : in String;
      Content      : in String);
   --  Send mail message to Email address

   procedure Update_RSS;
   --  Update RSS file

end Savadur.Notifications;
