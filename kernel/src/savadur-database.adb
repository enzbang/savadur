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

with Ada.Task_Attributes;
with Ada.Directories;

with DB.SQLite;
with DB.Tools;

with Savadur.Config;
with Savadur.Logs;
with Ada.Exceptions;

package body Savadur.Database is

   use Ada;
   use Ada.Exceptions;

   Module : constant Logs.Handler.Module_Name := "DB";

   type TLS_DBH is record
      Handle    : access DB.Handle'Class;
      Connected : Boolean;
   end record;

   type TLS_DBH_Access is access all TLS_DBH;

   Null_DBH : constant TLS_DBH :=
                TLS_DBH'(Handle => null, Connected => False);

   package DBH_TLS is
     new Task_Attributes (Attribute => TLS_DBH, Initial_Value => Null_DBH);

   procedure Connect (DBH : in TLS_DBH_Access);
   --  Connects to the database if needed

   -------------
   -- Connect --
   -------------

   procedure Connect (DBH : in TLS_DBH_Access) is
      DB_Path : constant String := Directories.Compose
        (Containing_Directory => Config.Savadur_Directory,
         Name                 => "logs",
         Extension            => "db");
   begin
      if not DBH.Connected then
         if Directories.Exists (Name => DB_Path) then
            DBH.Handle := new DB.SQLite.Handle;
            DBH.Handle.Connect (DB_Path);
            DBH.Connected := True;
            DBH_TLS.Set_Value (DBH.all);
         else
            Logs.Handler.Write
              (Name    => Module,
               Kind    => Logs.Handler.Error,
               Content => "ERROR : No database found : " & DB_Path &
              " Please run ./scripts/create_database");
            raise No_Database
              with "ERROR : No database found : " & DB_Path;
         end if;
      end if;
   end Connect;

   ------------------
   -- Final_Status --
   ------------------

   procedure Final_Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Result       : in Boolean)
   is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "insert into lastbuilt (project, client, "
        & "scenario, status) values (" & Q (Project_Name) & ", " & Q (Key)
        & ", " & Q (Scenario) & ", " & Q (Result) & ")";
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
     (Project_Name : in String) return Templates.Translate_Set is
      use type Templates.Tag;
      DBH      : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Iter     : DB.SQLite.Iterator;
      Line     : DB.String_Vectors.Vector;
      Set      : Templates.Translate_Set;
      Client   : Templates.Tag;
      Scenario : Templates.Tag;
      Status   : Templates.Tag;
      Date     : Templates.Tag;
   begin
      Connect (DBH);

      DBH.Handle.Prepare_Select
        (Iter, "select client, scenario, status, date from lastbuilt "
           & "where project = " & DB.Tools.Q (Project_Name)
           & " order by date DESC limit 50");

      while Iter.More loop
         Iter.Get_Line (Line);

         Client   := Client & DB.String_Vectors.Element (Line, 1);
         Scenario := Scenario & DB.String_Vectors.Element (Line, 2);
         Status   := Status & DB.String_Vectors.Element (Line, 3);
         Date     := Date & DB.String_Vectors.Element (Line, 4);

         Line.Clear;
      end loop;

      Iter.End_Select;

      Templates.Insert (Set, Templates.Assoc ("CLIENT", Client));
      Templates.Insert (Set, Templates.Assoc ("SCENARIO", Scenario));
      Templates.Insert (Set, Templates.Assoc ("STATUS", Status));
      Templates.Insert (Set, Templates.Assoc ("DATE", Date));

      return Set;
   end Get_Final_Status;

   ---------
   -- Log --
   ---------

   procedure Log
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Output       : in String;
      Result       : in Boolean)
   is
      use DB.Tools;

      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      SQL : constant String := "insert into logs (project, client, scenario,"
        & "action, log, status) values (" & Q (Project_Name) & ", " & Q (Key)
        & ", " & Q (Scenario) & ", " & Q (Action)
        & ", " & Q (Output) & ", " & Q (Result) & ")";
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);

   end Log;

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
      --  Set logout_date to null

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
      SQL : constant String := "update sessions set logout_date=now where"
        & " client = " & Q (Key);
   begin
      Connect (DBH);
      DBH.Handle.Execute (SQL);
   exception
      when E : DB.DB_Error =>
         Logs.Handler.Write (Name    => Module,
                             Content => Exception_Message (E),
                             Kind    => Logs.Handler.Error);

   end Logout;

end Savadur.Database;
