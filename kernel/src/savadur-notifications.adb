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

with Ada.Exceptions;

with AWS.Jabber;

with Savadur.Config.Notifications;
with Savadur.Logs;

package body Savadur.Notifications is

   use Ada;

   -----------
   -- Image --
   -----------

   function Image (H : in Hooks) return String is
   begin
      return "Notification :" & ASCII.LF
        & "On Success = " & Actions.Image (H.On_Success) & ASCII.LF
        & "On Failure = " & Actions.Image (H.On_Failure);
   end Image;

   ---------------
   -- Send_Mail --
   ---------------

   procedure Send_Mail
     (Email        : in String;
      Subject      : in String;
      Content      : in String)
   is
      pragma Unreferenced (Email, Subject, Content);
   begin
      null;
   end Send_Mail;

   ---------------
   -- XMPP_Send --
   ---------------

   procedure XMPP_Send
     (JID          : in String;
      Subject      : in String;
      Content      : in String)
   is
      Server : AWS.Jabber.Server;
   begin

      AWS.Jabber.Connect (Server    => Server,
                          Host      => Config.Notifications.Jabber_Server,
                          User      => Config.Notifications.Jabber_JID,
                          Password  => Config.Notifications.Jabber_Password,
                          Auth_Type => Config.Notifications.Jabber_Auth_Type);

      AWS.Jabber.Send_Message
        (Server => Server,
         JID    => JID,
         Subject => Subject,
         Content => Content);

      AWS.Jabber.Close (Server);
   exception
      when E : others =>
         Logs.Write ("Unknown_Error " & Exceptions.Exception_Information (E),
                     Logs.Handler.Error);
   end XMPP_Send;

end Savadur.Notifications;
