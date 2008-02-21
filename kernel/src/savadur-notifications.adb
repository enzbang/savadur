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
with Ada.Directories;
with Ada.Text_IO;

with AWS.Jabber;
with AWS.SMTP.Authentication.Plain;
with AWS.SMTP.Client;
with AWS.Templates;

with Savadur.Config.Notifications.SMTP;
with Savadur.Config.Notifications.XMPP;
with Savadur.Config.Syndication;
with Savadur.Database;
with Savadur.Logs;

package body Savadur.Notifications is

   use Ada;
   use AWS;

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
     (Email   : in String;
      Subject : in String;
      Content : in String)
   is
      SMTP_Server : SMTP.Receiver;
      Auth        : aliased SMTP.Authentication.Plain.Credential;
      Result      : SMTP.Status;
   begin
      if Config.Notifications.SMTP.User /= "" then
         Auth := SMTP.Authentication.Plain.Initialize
           (Config.Notifications.SMTP.User,
            Config.Notifications.SMTP.Password);

         SMTP_Server := SMTP.Client.Initialize
           (Config.Notifications.SMTP.Server, Credential => Auth'Access);

      else
         SMTP_Server := SMTP.Client.Initialize
           (Config.Notifications.SMTP.Server);
      end if;

      SMTP.Client.Send
        (Server  => SMTP_Server,
         From    => SMTP.E_Mail ("savadur", "no-reply"),
         To      => SMTP.E_Mail (Email, Email),
         Subject => Subject,
         Message => Content,
         Status  => Result);

      if not SMTP.Is_Ok (Result) then
         Logs.Write
           ("Send_Mail error : (TO=" & Email & ", subject=" & Subject & ")",
            Logs.Handler.Error);
      end if;
   end Send_Mail;

   ----------------
   -- Update_RSS --
   ----------------

   procedure Update_RSS is
      Web_Dir       : constant String := Directories.Compose
        (Containing_Directory => Savadur.Config.Savadur_Directory,
         Name                 => "htdocs");
      RSS_DIR       : constant String := Directories.Compose
        (Containing_Directory => Web_Dir,
         Name                 => "rss");
      Web_Templates : constant String := Directories.Compose
        (Containing_Directory => Web_Dir,
         Name                 => "templates");
      Set  : Templates.Translate_Set := Savadur.Database.Get_Final_Status;
      File : Text_IO.File_Type;
   begin
      Templates.Insert
        (Set, Templates.Assoc ("CHANNEL_TITLE",
         Savadur.Config.Syndication.Channel_Title));
      Templates.Insert
        (Set, Templates.Assoc ("CHANNEL_DESCRIPTION",
         "Savadur, last built information for all registered projects"));
      Templates.Insert
        (Set, Templates.Assoc ("CHANNEL_LINK",
         Savadur.Config.Syndication.Channel_Link));

      Text_IO.Put_Line
        (Directories.Compose
           (Containing_Directory => RSS_DIR,
            Name                 => "all",
            Extension            => "xml"));

      Text_IO.Create (File => File,
              Mode => Text_IO.Out_File,
              Name => Directories.Compose
                (Containing_Directory => RSS_DIR,
                 Name                 => "all",
                 Extension            => "xml"));

      Text_IO.Put
        (File => File,
         Item => Templates.Parse
           (Filename     => Directories.Compose
              (Containing_Directory => Web_Templates,
               Name                 => "rss2",
               Extension            => "txml"),
            Translations  => Set));

      Text_IO.Close (File);
   end Update_RSS;

   ---------------
   -- XMPP_Send --
   ---------------

   procedure XMPP_Send
     (JID     : in String;
      Subject : in String;
      Content : in String)
   is
      Server : AWS.Jabber.Server;
   begin
      Jabber.Connect
        (Server    => Server,
         Host      => Config.Notifications.XMPP.Jabber_Server,
         User      => Config.Notifications.XMPP.Jabber_JID,
         Password  => Config.Notifications.XMPP.Jabber_Password,
         Auth_Type => Config.Notifications.XMPP.Jabber_Auth_Type);

      Jabber.Send_Message
        (Server => Server,
         JID    => JID,
         Subject => Subject,
         Content => Content);

      Jabber.Close (Server);
   exception
      when E : others =>
         Logs.Write ("Unknown_Error " & Exceptions.Exception_Information (E),
                     Logs.Handler.Error);
   end XMPP_Send;

end Savadur.Notifications;
