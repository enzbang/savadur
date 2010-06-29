------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2010                          --
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

with AWS.Jabber.Client;
with AWS.SMTP.Authentication.Plain;
with AWS.SMTP.Client;
with AWS.Templates;

with Savadur.Config.Notifications;
with Savadur.Config.Syndication;
with Savadur.Database;
with Savadur.Logs;

package body Savadur.Notifications is

   use Ada;
   use AWS;

   RSS_All_File      : access String := null;
   RSS_Template_File : access String := null;

   Jabber_Is_Connected : Boolean := False;
   Account : aliased Jabber.Client.Account;

   procedure Jabber_Connect (Account : Jabber.Client.Account_Access);
   --  Connect to the jabber account

   -----------
   -- Image --
   -----------

   function Image (H : in Hooks) return String is
   begin
      return "Notification :" & ASCII.LF
        & "On Success = " & Actions.Image (H.On_Success) & ASCII.LF
        & "On Failure = " & Actions.Image (H.On_Failure);
   end Image;

   --------------------
   -- Jabber_Connect --
   --------------------

   procedure Jabber_Connect (Account : Jabber.Client.Account_Access) is
   begin
      Jabber.Client.Set_Host (Account => Account.all,
                              Host    => Config.Notifications.XMPP.Server);

      Jabber.Client.Set_Login_Information
        (Account  => Account.all,
         User     => Config.Notifications.XMPP.JID,
         Password => Config.Notifications.XMPP.Password,
         Resource => "savadur");

      Jabber.Client.Set_Authentication_Type
        (Account   => Account.all,
         Auth_Type => Config.Notifications.XMPP.Auth_Type);

      Jabber.Client.Connect (Account.all);

      Jabber_Is_Connected := True;
   end Jabber_Connect;

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
         From    => SMTP.E_Mail (Config.Notifications.SMTP.Sender,
           Config.Notifications.SMTP.Sender),
         To      => SMTP.E_Mail (Email, Email),
         Subject => Subject,
         Message => Content,
         Status  => Result);

      if not SMTP.Is_Ok (Result) then
         Logs.Write
           (Content => "Send_Mail error : (TO=" & Email
            & ", subject=" & Subject & ")",
            Kind    => Logs.Handler.Error);
      end if;
   end Send_Mail;

   ----------------
   -- Update_RSS --
   ----------------

   procedure Update_RSS is
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

      if RSS_All_File = null then
         RSS_All_File := new String'
           (Directories.Compose
              (Containing_Directory => Config.RSS_Directory,
               Name                 => "all",
               Extension            => "xml"));

         if not Directories.Exists (Config.RSS_Directory) then
            Directories.Create_Path (Config.RSS_Directory);
         end if;
      end if;

      if RSS_Template_File = null then
         RSS_Template_File := new String'
           (Directories.Compose
              (Containing_Directory => Config.Web_Templates_Directory,
               Name                 => "rss2",
               Extension            => "txml"));
      end if;

      if Directories.Exists (RSS_Template_File.all) then
         Text_IO.Create
           (File => File,
            Mode => Text_IO.Out_File,
            Name => RSS_All_File.all);

         Text_IO.Put
           (File => File,
            Item => Templates.Parse
              (Filename      => RSS_Template_File.all,
               Translations  => Set));

         Text_IO.Close (File);

      else
         Logs.Write
           (Content => "Cannot find RSS template: " & RSS_Template_File.all,
            Kind    => Logs.Handler.Warnings);
      end if;
   end Update_RSS;

   ---------------
   -- XMPP_Send --
   ---------------

   procedure XMPP_Send
     (JID     : in String;
      Subject : in String;
      Content : in String)
   is
   begin
      if not Jabber_Is_Connected then
         Jabber_Connect (Account'Access);
      end if;

      Jabber.Client.Send (Account      => Account,
                          JID          => Jabber.Client.Jabber_ID (JID),
                          Content      => Content,
                          Subject      => Subject,
                          Message_Type => Jabber.Client.M_Normal);
      Jabber.Client.Close (Account);
   exception
      when E : others =>
         Logs.Write
           (Content => "Unknown_Error " & Exceptions.Exception_Information (E),
            Kind    => Logs.Handler.Error);
   end XMPP_Send;

end Savadur.Notifications;
