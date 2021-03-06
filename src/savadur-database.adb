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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Task_Attributes;

with DB.SQLite;
with DB.Tools;

with Morzhol.Strings;

with Savadur.Config;
with Savadur.Logs;
with Savadur.Utils;

package body Savadur.Database is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   Module : constant Logs.Handler.Module_Name := "DB";

   type TLS_DBH is record
      Handle    : access DB.Handle'Class;
      Connected : Boolean;
   end record;

   type TLS_DBH_Access is access all TLS_DBH;

   Null_DBH : constant TLS_DBH :=
                TLS_DBH'(Handle => null, Connected => False);

   DB_Path : access String := null;

   package DBH_TLS is
     new Task_Attributes (Attribute => TLS_DBH, Initial_Value => Null_DBH);

   procedure Connect (DBH : in TLS_DBH_Access);
   --  Connects to the database if needed

   ----------------------
   -- Add_Notification --
   ----------------------

   procedure Add_Notification
     (Project_Name      : in String;
      E_Mail, Log_Email : in String;
      XMPP, Log_XMPP    : in String)
   is
      function TF (Str : in String) return Boolean;
      --  Returns true/false for the checkboxes

      --------
      -- TF --
      --------

      function TF (Str : in String) return Boolean is
      begin
         if Str = "" then
            return False;
         else
            return True;
         end if;
      end TF;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);

      DBH.Handle.Execute
        ("insert into notify "
         & "(project, email, log_email, xmpp, log_xmpp) "
         & " values ("
         & DB.Tools.Q (Project_Name)
         & ", " & DB.Tools.Q (E_Mail)
         & ", " & DB.Tools.Q (TF (Log_Email))
         & ", " & DB.Tools.Q (XMPP)
         & ", " & DB.Tools.Q (TF (Log_XMPP)) & ")");
   end Add_Notification;

   -------------
   -- Connect --
   -------------

   procedure Connect (DBH : in TLS_DBH_Access) is
   begin
      if DB_Path = null then
         DB_Path := new String'
           (Directories.Compose
              (Containing_Directory => Config.Savadur_Directory,
               Name                 => "logs",
               Extension            => "db"));

         if not Directories.Exists (Name => DB_Path.all) then
            Logs.Handler.Write
              (Name    => Module,
               Kind    => Logs.Handler.Error,
               Content => "ERROR : No database found : " & DB_Path.all &
               " Please run ./scripts/create_database");
            raise No_Database
              with "ERROR : No database found : " & DB_Path.all;
         end if;
      end if;

      if not DBH.Connected then
            DBH.Handle := new DB.SQLite.Handle;
            DBH.Handle.Connect (DB_Path.all);
            DBH.Connected := True;
            DBH_TLS.Set_Value (DBH.all);
      end if;
   end Connect;

   ----------------------
   -- Del_Notification --
   ----------------------

   procedure Del_Notification
     (Project_Name : in String;
      E_Mail, XMPP : in String)
   is
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);

      function Not_Empty (Name, Value : in String) return String;

      ---------------
      -- Not_Empty --
      ---------------

      function Not_Empty (Name, Value : in String) return String is
      begin
         if Value = "" then
            return "";
         else
            return "and " & Name & "=" & DB.Tools.Q (Value);
         end if;
      end Not_Empty;

   begin
      Connect (DBH);

      DBH.Handle.Execute
        ("delete from notify where"
         & " project=" & DB.Tools.Q (Project_Name)
         & Not_Empty ("email", E_Mail)
         & Not_Empty ("xmpp", XMPP));
   end Del_Notification;

   ------------------
   -- Final_Status --
   ------------------

   procedure Final_Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Result       : in Boolean;
      Job_Id       : in Natural)
   is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "insert into lastbuilt (project, client, "
        & "scenario, status, job_id) values ("
        & Q (Project_Name) & ", " & Q (Key)
        & ", " & Q (Scenario) & ", " & Q (Result)
        & ", " & I (Job_Id) & ")";
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);
   end Final_Status;

   ------------------------
   --  Get_Final_Status  --
   ------------------------

   function Get_Final_Status
     (Project_Name : in String := "") return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH       : constant TLS_DBH_Access :=
                    TLS_DBH_Access (DBH_TLS.Reference);
      SQL_Sel   : constant String :=
                   "select client, scenario, status, "
                      & "datetime(max(date), 'localtime'), job_id, project"
                      & " from lastbuilt ";
      SQL_Group : constant String := " group by client order by client";
      Iter      : DB.SQLite.Iterator;
      Line      : DB.String_Vectors.Vector;
      Set       : Templates.Translate_Set;
      Name      : Templates.Tag;
      Client    : Templates.Tag;
      Scenario  : Templates.Tag;
      Status    : Templates.Tag;
      Date      : Templates.Tag;
      Job_Id    : Templates.Tag;
   begin
      Connect (DBH);

      if Project_Name /= "" then
         DBH.Handle.Prepare_Select
           (Iter, SQL_Sel & "where project = " & DB.Tools.Q (Project_Name)
            & SQL_Group);
      else
         DBH.Handle.Prepare_Select
           (Iter, SQL_Sel & SQL_Group);
      end if;

      while Iter.More loop
         Iter.Get_Line (Line);

         Client   := Client & DB.String_Vectors.Element (Line, 1);
         Scenario := Scenario & DB.String_Vectors.Element (Line, 2);
         Status   := Status & DB.String_Vectors.Element (Line, 3);
         Date     := Date & DB.String_Vectors.Element (Line, 4);
         Job_Id   := Job_Id & DB.String_Vectors.Element (Line, 5);
         Name     := Name & DB.String_Vectors.Element (Line, 6);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc ("CLIENT", Client));
      Templates.Insert (Set, Templates.Assoc ("SCENARIO", Scenario));
      Templates.Insert (Set, Templates.Assoc ("STATUS", Status));
      Templates.Insert (Set, Templates.Assoc ("DATE", Date));
      Templates.Insert (Set, Templates.Assoc ("JOB_ID", Job_Id));
      Templates.Insert (Set, Templates.Assoc ("NAME", Name));

      return Set;
   end Get_Final_Status;

   -----------------------
   --  Get_Log_Content  --
   -----------------------

   function Get_Log_Content
     (Id : in Positive) return Templates.Translate_Set
   is
      use Savadur.Utils;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.SQLite.Iterator;
      Line : DB.String_Vectors.Vector;
      Set  : Templates.Translate_Set;
      Log  : Unbounded_String;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select filename, log, project, scenario, status,"
         & " datetime(start_date, 'localtime'),"
         & " datetime(stop_date, 'localtime'), case"
         & " when stop_date is null then"
         & " (last_duration -(julianday(datetime('now'))*86000 -"
         & " julianday(start_date)*86000)) else duration end,"
         & " action"
         & " from logs where rowid = " & DB.Tools.I (Id));

      if Iter.More then
         Iter.Get_Line (Line);

         Get_Content : begin
            --  Get the log content in priority from the specified filename as
            --  the log is not cut here. If filename is not defined then return
            --  the stored log in the database.

            if DB.String_Vectors.Element (Line, 1) /= "" then
               Log := +Utils.Content (DB.String_Vectors.Element (Line, 1));
            else
               Log := +DB.String_Vectors.Element (Line, 2);
            end if;

            Templates.Insert
              (Set, Templates.Assoc
                 ("LOG_CONTENT", Morzhol.Strings.HTML_To_Text (-Log)));
            Templates.Insert
              (Set, Templates.Assoc
                 ("PROJECT_NAME", DB.String_Vectors.Element (Line, 3)));
            Templates.Insert
              (Set, Templates.Assoc
                 ("SCENARIO", DB.String_Vectors.Element (Line, 4)));
            Templates.Insert
              (Set, Templates.Assoc
                 ("STATUS", DB.String_Vectors.Element (Line, 5)));
            Templates.Insert
              (Set, Templates.Assoc
                 ("START_DATE", DB.String_Vectors.Element (Line, 6)));
            Templates.Insert
              (Set, Templates.Assoc
                 ("STOP_DATE", DB.String_Vectors.Element (Line, 7)));

            Compute_Duration : declare
               Get_Duration : constant String :=
                                DB.String_Vectors.Element (Line, 8);
               Duration     : Natural := 0;
            begin
               if Get_Duration /= ""
                 and then Float'Value (Get_Duration) > 0.0
               then
                  Duration := Natural
                    (Float'Rounding (Float'Value (Get_Duration)));
               end if;
               Templates.Insert
                 (Set, Templates.Assoc
                    ("DURATION", Natural'Image (Duration)));
            end Compute_Duration;

            Templates.Insert
              (Set, Templates.Assoc
                 ("ACTION", DB.String_Vectors.Element (Line, 9)));

            Line.Clear;
            Iter.End_Select;
            return Set;
         end Get_Content;
      end if;

      Iter.End_Select;
      return Set;
   end Get_Log_Content;

   ----------------
   --  Get_Logs  --
   ----------------

   function Get_Logs
     (Project_Name : in String;
      Log_Size     : in Natural) return Templates.Translate_Set
   is
      use type Templates.Tag;
      DBH           : constant TLS_DBH_Access :=
                          TLS_DBH_Access (DBH_TLS.Reference);
      Iter          : DB.SQLite.Iterator;
      Line          : DB.String_Vectors.Vector;
      Set           : Templates.Translate_Set;
      Client        : Templates.Tag;
      Scenario      : Templates.Tag;
      Scenario_Date : Templates.Tag;
      Action        : Templates.Tag;
      Status        : Templates.Tag;
      Start_Date    : Templates.Tag;
      Stop_Date     : Templates.Tag;
      Duration      : Templates.Tag;
      Job_Id        : Templates.Tag;
      Rowid         : Templates.Tag;

      --  Remember last client and his scenarios

      Last_Client        : Unbounded_String;
      Last_Scenario      : Unbounded_String;
      Last_Scenario_Date : Unbounded_String;
      Last_Job_Id        : Natural := 0;

      --  Temporay Tag for a client

      Scenario_Client      : Templates.Tag;
      Scenario_Date_Client : Templates.Tag;
      Status_Client        : Templates.Tag;
      Start_Date_Client    : Templates.Tag;
      Stop_Date_Client     : Templates.Tag;
      Duration_Client      : Templates.Tag;
      Action_Client        : Templates.Tag;
      Rowid_Client         : Templates.Tag;
      Job_Id_Client        : Templates.Tag;

      --  Temporay Tag for a job id

      Rowid_Job_Id      : Templates.Tag;
      Status_Job_Id     : Templates.Tag;
      Start_Date_Job_Id : Templates.Tag;
      Stop_Date_Job_Id  : Templates.Tag;
      Duration_Job_Id   : Templates.Tag;
      Action_Job_Id     : Templates.Tag;

      Size              : Natural := 0;

   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select client, scenario, status, "
         & "datetime(start_date, 'localtime'), "
         & "datetime(stop_date, 'localtime'), case"
         & " when stop_date is null then"
         & " (last_duration -(julianday(datetime('now'))*86000 -"
         & " julianday(start_date)*86000)) else duration end,"
         & " action, job_id, rowid from logs"
         & " where project = " & DB.Tools.Q (Project_Name)
         & " order by client ASC, rowid DESC");

      while Iter.More loop
         Iter.Get_Line (Line);

         --  We are parsing a new client

         if To_String (Last_Client) /= ""
           and then
             To_String (Last_Client) /= DB.String_Vectors.Element (Line, 1)
         then
            Size := 0;

            Job_Id_Client        := Job_Id_Client & Last_Job_Id;
            Scenario_Client      := Scenario_Client & Last_Scenario;
            Scenario_Date_Client := Scenario_Date_Client & Last_Scenario_Date;

            --  Reset last Job_ID

            Last_Job_Id := 0;

            Status_Client     := Status_Client     & Status_Job_Id;
            Start_Date_Client := Start_Date_Client & Start_Date_Job_Id;
            Stop_Date_Client  := Stop_Date_Client  & Stop_Date_Job_Id;
            Duration_Client   := Duration_Client   & Duration_Job_Id;
            Action_Client     := Action_Client     & Action_Job_Id;
            Rowid_Client      := Rowid_Client      & Rowid_Job_Id;

            --  Clear temp tags

            Templates.Clear (Status_Job_Id);
            Templates.Clear (Start_Date_Job_Id);
            Templates.Clear (Stop_Date_Job_Id);
            Templates.Clear (Duration_Job_Id);
            Templates.Clear (Action_Job_Id);
            Templates.Clear (Rowid_Job_Id);

            --  New client column

            Client        := Client        & Last_Client;
            Scenario      := Scenario      & Scenario_Client;
            Scenario_Date := Scenario_Date & Scenario_Date_Client;
            Status        := Status        & Status_Client;
            Start_Date    := Start_Date    & Start_Date_Client;
            Stop_Date     := Stop_Date     & Stop_Date_Client;
            Duration      := Duration      & Duration_Client;
            Action        := Action        & Action_Client;
            Rowid         := Rowid         & Rowid_Client;
            Job_Id        := Job_Id        & Job_Id_Client;

            --  Clear temp tags

            Templates.Clear (Scenario_Client);
            Templates.Clear (Scenario_Date_Client);
            Templates.Clear (Status_Client);
            Templates.Clear (Start_Date_Client);
            Templates.Clear (Stop_Date_Client);
            Templates.Clear (Duration_Client);
            Templates.Clear (Action_Client);
            Templates.Clear (Rowid_Client);
            Templates.Clear (Job_Id_Client);
         end if;

            --  We are parsing a new job

         if Last_Job_Id /= 0
           and then Last_Job_Id /=
             Natural'Value (DB.String_Vectors.Element (Line, 8))
         then
            Size := Size + 1;

            if Size < Log_Size then
               Job_Id_Client        := Job_Id_Client        & Last_Job_Id;
               Scenario_Client      := Scenario_Client      & Last_Scenario;
               Scenario_Date_Client :=
                 Scenario_Date_Client & Last_Scenario_Date;

               --  Reset last Job_ID

               Last_Job_Id := 0;

               Status_Client     := Status_Client     & Status_Job_Id;
               Start_Date_Client := Start_Date_Client & Start_Date_Job_Id;
               Stop_Date_Client  := Stop_Date_Client  & Stop_Date_Job_Id;
               Duration_Client   := Duration_Client   & Duration_Job_Id;
               Action_Client     := Action_Client     & Action_Job_Id;
               Rowid_Client      := Rowid_Client      & Rowid_Job_Id;

               --  Clear temp tags

               Templates.Clear (Status_Job_Id);
               Templates.Clear (Start_Date_Job_Id);
               Templates.Clear (Stop_Date_Job_Id);
               Templates.Clear (Duration_Job_Id);
               Templates.Clear (Action_Job_Id);
               Templates.Clear (Rowid_Job_Id);
            end if;
         end if;

         if Size < Log_Size then
            Status_Job_Id     := Status_Job_Id
              & DB.String_Vectors.Element (Line, 3);
            Start_Date_Job_Id := Start_Date_Job_Id
              & DB.String_Vectors.Element (Line, 4);
            Stop_Date_Job_Id  := Stop_Date_Job_Id
              & DB.String_Vectors.Element (Line, 5);

            Set_Duration : declare
               Get_Duration : constant String :=
                                DB.String_Vectors.Element (Line, 6);
               Duration     : Natural := 0;
            begin
               if Get_Duration /= ""
                 and then Float'Value (Get_Duration) > 0.0
               then
                  Duration := Natural
                    (Float'Rounding (Float'Value (Get_Duration)));
               end if;

               Duration_Job_Id := Duration_Job_Id & Natural'Image (Duration);
            end Set_Duration;

            Action_Job_Id     := Action_Job_Id
              & DB.String_Vectors.Element (Line, 7);
            Rowid_Job_Id      := Rowid_Job_Id
              & DB.String_Vectors.Element (Line, 9);

            Last_Client        := To_Unbounded_String
              (DB.String_Vectors.Element (Line, 1));
            Last_Scenario      := To_Unbounded_String
              (DB.String_Vectors.Element (Line, 2));
            Last_Scenario_Date := To_Unbounded_String
              (DB.String_Vectors.Element (Line, 4));
            Last_Job_Id        := Natural'Value
              (DB.String_Vectors.Element (Line, 8));
         end if;

         Line.Clear;
      end loop;

      if To_String (Last_Client) /= "" then
         Job_Id_Client        := Job_Id_Client        & Last_Job_Id;
         Scenario_Client      := Scenario_Client      & Last_Scenario;
         Scenario_Date_Client := Scenario_Date_Client & Last_Scenario_Date;
         Status_Client        := Status_Client        & Status_Job_Id;
         Start_Date_Client    := Start_Date_Client    & Start_Date_Job_Id;
         Stop_Date_Client     := Stop_Date_Client     & Stop_Date_Job_Id;
         Duration_Client      := Duration_Client      & Duration_Job_Id;
         Action_Client        := Action_Client        & Action_Job_Id;
         Rowid_Client         := Rowid_Client         & Rowid_Job_Id;

         --  New client column

         Client        := Client        & Last_Client;
         Scenario      := Scenario      & Scenario_Client;
         Scenario_Date := Scenario_Date & Scenario_Date_Client;
         Status        := Status        & Status_Client;
         Start_Date    := Start_Date    & Start_Date_Client;
         Stop_Date     := Stop_Date     & Stop_Date_Client;
         Duration      := Duration      & Duration_Client;
         Action        := Action        & Action_Client;
         Rowid         := Rowid         & Rowid_Client;
         Job_Id        := Job_Id        & Job_Id_Client;
      end if;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc ("LOGS_CLIENT", Client));
      Templates.Insert (Set, Templates.Assoc ("LOGS_SCENARIO", Scenario));
      Templates.Insert
        (Set, Templates.Assoc ("LOGS_SCENARIO_START_DATE", Scenario_Date));
      Templates.Insert (Set, Templates.Assoc ("LOGS_STATUS", Status));
      Templates.Insert (Set, Templates.Assoc ("LOGS_START_DATE", Start_Date));
      Templates.Insert (Set, Templates.Assoc ("LOGS_STOP_DATE", Stop_Date));
      Templates.Insert (Set, Templates.Assoc ("LOGS_DURATION", Duration));
      Templates.Insert (Set, Templates.Assoc ("LOGS_ACTION", Action));
      Templates.Insert (Set, Templates.Assoc ("LOGS_ID", Rowid));
      Templates.Insert (Set, Templates.Assoc ("LOGS_JOB_ID", Job_Id));

      return Set;
   end Get_Logs;

   -----------------------
   -- Get_Notifications --
   -----------------------

   function Get_Notifications
     (Project_Name : in String) return Templates.Translate_Set
   is
      use type Templates.Tag;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.SQLite.Iterator;
      Line : DB.String_Vectors.Vector;

      Set        : Templates.Translate_Set;
      E_Mail     : Templates.Tag;
      Log_Email  : Templates.Tag;
      Jabber     : Templates.Tag;
      Log_Jabber : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter,
         "select email, log_email, xmpp, log_xmpp from notify where project="
         & DB.Tools.Q (Project_Name));

      while Iter.More loop
         Iter.Get_Line (Line);

         E_Mail    := E_Mail & DB.String_Vectors.Element (Line, 1);
         Log_Email := Log_Email & DB.String_Vectors.Element (Line, 2);
         Jabber := Jabber & DB.String_Vectors.Element (Line, 3);
         Log_Jabber := Log_Jabber & DB.String_Vectors.Element (Line, 4);
         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc ("E_MAIL", E_Mail));
      Templates.Insert (Set, Templates.Assoc ("LOG_EMAIL", Log_Email));
      Templates.Insert (Set, Templates.Assoc ("JABBER", Jabber));
      Templates.Insert (Set, Templates.Assoc ("LOG_JABBER", Log_Jabber));

      return Set;
   end Get_Notifications;

   --------------
   --  Job_Id  --
   --------------

   function Job_Id return Positive is
      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.SQLite.Iterator;
      Line : DB.String_Vectors.Vector;
      Id   : Positive;
   begin
      Connect (DBH);

      --  Create new job id
      --  As job id is "primary key autoincrement" insert a NULL value

      DBH.Handle.Execute ("insert into job_id values (NULL)");

      --  Now select the last value

      DBH.Handle.Prepare_Select (Iter, "select max(id) from job_id");

      if Iter.More then
         Iter.Get_Line (Line);

         Id := Positive'Value (DB.String_Vectors.Element (Line, 1));

         Line.Clear;
      end if;

      Iter.End_Select;

      return Id;
   end Job_Id;

   ---------
   -- Log --
   ---------

   procedure Log
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Filename     : in String;
      Output       : in String;
      Result       : in Natural;
      Job_Id       : in Natural)
   is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "update logs set log = "
        & Q (Output) & ", filename = " & Q (Filename)
        & ", status = " & I (Result) & ", stop_date = datetime('now')"
        & " where project = " & Q (Project_Name) & " and client = " & Q (Key)
        & " and scenario = " & Q (Scenario) & " and action = " & Q (Action)
        & " and job_id = " & I (Job_Id);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);
   end Log;

   ---------------
   -- Log_Start --
   ---------------

   procedure Log_Start
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Job_Id       : in Natural)
   is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "insert into logs (project, client, scenario,"
        & "action, job_id) values ("
        & Q (Project_Name) & ", " & Q (Key)
        & ", " & Q (Scenario) & ", " & Q (Action)
        & ", " & I (Job_Id) & ")";
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);
   end Log_Start;

   -----------
   -- Login --
   -----------

   procedure Login (Key : in String) is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "insert into sessions (client, logout_date)"
        & " values (" & Q (Key) & ", NULL)";
   begin
      --  Login date is set by default to current_timestamp
      --  Set logout_date to null.

      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);
   end Login;

   ------------
   -- Logout --
   ------------

   procedure Logout (Key : in String) is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "update sessions set "
        & "logout_date=current_timestamp where logout_date "
        & "is NULL and client=" & Q (Key);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);
   end Logout;

   ------------------------
   -- Send_Notifications --
   ------------------------

   procedure Send_Notifications
     (Project_Name   : in String;
      Send_Mail_Hook : in Send_Mail;
      Send_XMPP_Hook : in Send_XMPP;
      Subject        : in String;
      Content        : in String;
      Output         : in String)
   is

      function Full_Content
        (Content : in String;
         Output  : in String;
         Log     : in Boolean) return String;
      --  Add Log to the given content

      ------------------
      -- Full_Content --
      ------------------

      function Full_Content
        (Content : in String;
         Output  : in String;
         Log     : in Boolean) return String is
      begin
         if Log then
            return Content & ASCII.LF & ASCII.LF & "--- " & ASCII.LF
              & "Log content:" & ASCII.LF & ASCII.LF & Output & ASCII.LF;
         else
            return Content;
         end if;
      end Full_Content;

      DBH  : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter : DB.SQLite.Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select email, log_email, xmpp, log_xmpp"
         & " from notify where project = " & DB.Tools.Q (Project_Name));

      while Iter.More loop
         Iter.Get_Line (Line);

         Run_Hooks : declare
            Email     : constant String := DB.String_Vectors.Element (Line, 1);
            Log_Email : constant Boolean :=
                          Boolean'Value (DB.String_Vectors.Element (Line, 2));
            XMPP      : constant String := DB.String_Vectors.Element (Line, 3);
            Log_XMPP  : constant Boolean :=
                          Boolean'Value (DB.String_Vectors.Element (Line, 4));
         begin
            if Email /= "" then
               Logs.Handler.Write (Name    => Module,
                                   Content => "Send mail to " & Email,
                                   Kind    => Logs.Handler.Very_Verbose);
               Send_Mail_Hook
                 (Email, Subject,
                  Full_Content (Content, Output, Log_Email));
            end if;

            if XMPP /= "" then
               Logs.Handler.Write (Name    => Module,
                                   Content => "Send jabber message to " & XMPP,
                                   Kind    => Logs.Handler.Very_Verbose);
                  Send_XMPP_Hook
                    (XMPP, Subject,
                     Full_Content (Content, Output, Log_XMPP));
            end if;

            Line.Clear;
         end Run_Hooks;
      end loop;

      Iter.End_Select;
   end Send_Notifications;

end Savadur.Database;
