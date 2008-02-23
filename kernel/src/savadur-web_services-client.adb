------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                        Copyright (C) 2007-2008                           --
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

with Ada.Containers.Ordered_Sets;
with Ada.Directories;

with Savadur.Clients;
with Savadur.Config.SCM;
with Savadur.Config.Project;
with Savadur.Database;
with Savadur.Logs;
with Savadur.Notifications;
with Savadur.Projects.Sets;
with Savadur.SCM;
with Savadur.Signed_Files;
with Savadur.Utils;

package body Savadur.Web_Services.Client is

   use Ada;
   use Savadur.Utils;

   task type Update_Status;

   Status_Updater : access Update_Status := null;

   type Report_Data is record
      Key          : Unbounded_String;
      Project_Name : Unbounded_String;
      Scenario     : Unbounded_String;
      Action       : Unbounded_String;
      Output       : Unbounded_String;
      Result       : Boolean;
      Job_Id       : Natural;
      Number       : Natural := 0;
   end record;

   function "<" (R1, R2 : in Report_Data) return Boolean;
   --  Returns True if R1 has been created before R2

   package Report_Set is new Containers.Ordered_Sets (Report_Data);

   protected Report_Handler is

      procedure Add (Report : in Report_Data);
      --  Adds a new report into the queue

      entry Get (Report : in out Report_Data);
      --  Gets the first report in the queue

   private
      Reports : Report_Set.Set;
      Size    : Natural := 0;
      Number  : Natural := 0;
   end Report_Handler;

   ---------
   -- "<" --
   ---------

   function "<" (R1, R2 : in Report_Data) return Boolean is
   begin
      return R1.Number < R2.Number;
   end "<";

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Signed_Project : in Client.Signed_Project) return File_Data
   is
      use type Signed_Files.Signature;

      Project      : aliased constant Signed_Files.Handler :=
                       Signed_Files.To_Handler
                         (Signed_Files.External_Handler (Signed_Project));
      Project_Name : constant String := Signed_Files.Name (Project);
   begin
      Logs.Write ("Load_Project " & Project_Name);

      if Projects.Sets.Keys.Contains
        (Config.Project.Configurations, Project_Name)
      then
         declare
            Proj             : aliased Projects.Project_Config :=
                                 Config.Project.Get (Project_Name);
            Project_Filename : constant String :=
                                 Projects.Project_Filename (Proj'Access);
            Local_Project    : aliased Signed_Files.Handler;
         begin
            Signed_Files.Create
              (Local_Project, Project_Name, Project_Filename);

            if Signed_Files.SHA1 (Local_Project) =
              Signed_Files.SHA1 (Project)
            then
               Logs.Write ("   up-to-date");
               --  Client up-to-date
               return No_File;

            else
               Logs.Write ("   content returned");
               return File_Data'
                 (Filename => +Directories.Simple_Name (Project_Filename),
                  Content  => +Utils.Content (Project_Filename));
            end if;
         end;

      else
         Logs.Write ("   project not found!");
         return No_File;
      end if;
   end Load_Project;

   --------------
   -- Load_SCM --
   --------------

   function Load_SCM (Signed_SCM : in Client.Signed_SCM) return File_Data is
      use SCM.Id_Utils;
      use type Signed_Files.Signature;

      S_SCM    : aliased constant Signed_Files.Handler :=
                   Signed_Files.To_Handler
                     (Signed_Files.External_Handler (Signed_SCM));
      SCM_Name : constant String := Signed_Files.Name (S_SCM);
   begin
      Logs.Write ("Load_SCM " & SCM_Name);

      if SCM.Keys.Contains
        (Config.SCM.Configurations, Value (SCM_Name))
      then
         declare
            S            : aliased constant SCM.SCM :=
                             Config.SCM.Get (SCM_Name);
            SCM_Filename : constant String := -S.Filename;
            Local_SCM    : aliased Signed_Files.Handler;
         begin
            Signed_Files.Create
              (Local_SCM, SCM_Name, SCM_Filename);

            if Signed_Files.SHA1 (Local_SCM) = Signed_Files.SHA1 (S_SCM) then
               Logs.Write ("   up-to-date");
               --  Client up-to-date
               return No_File;

            else
               Logs.Write ("   content returned");
               return File_Data'
                 (Filename => +Directories.Simple_Name (SCM_Filename),
                  Content  => +Utils.Content (SCM_Filename));
            end if;
         end;

      else
         Logs.Write ("   SCM not found!");
         return No_File;
      end if;
   end Load_SCM;

   ----------
   -- Ping --
   ----------

   function Ping return String is
   begin
      return "Pong";
   end Ping;

   --------------
   -- Register --
   --------------

   procedure Register
     (Key               : in String;
      Data              : in Metadata;
      Server_Name       : in String;
      Callback_Endpoint : in String) is
   begin
      Logs.Write ("Register new client : " & Key & ":"
                  & Server_Name & '@' & Callback_Endpoint);
      Logs.Write ("Client Metadata are OS = " & (-Data.OS),
                  Logs.Handler.Very_Verbose);

      Insert_Or_Update : begin
         Clients.Registered.Insert
           (New_Item => (+Key, Data, +Server_Name, +Callback_Endpoint));
      exception
         when Constraint_Error =>
            --  If the client has been deconnected and try to register again
            --  replace the old configuration by the new one
            Logs.Write ("Client exits... update it");
            Clients.Registered.Replace
              (New_Item => (+Key, Data, +Server_Name, +Callback_Endpoint));
      end Insert_Or_Update;

      Database.Login (Key);
   end Register;

   --------------------
   -- Report_Handler --
   --------------------

   protected body Report_Handler is

      ---------
      -- Add --
      ---------

      procedure Add (Report : in Report_Data) is
         New_Report : Report_Data := Report;
      begin
         Number := Number + 1;
         New_Report.Number := Number;
         Reports.Insert (New_Report);
         Size := Size + 1;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Report : in out Report_Data) when Size > 0 is
      begin
         Report := Reports.First_Element;
         Reports.Delete_First;
         Size := Size - 1;
      end Get;
   end Report_Handler;

   ------------
   -- Status --
   ------------

   procedure Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Output       : in String;
      Result       : in Boolean;
      Job_Id       : in Natural) is
   begin
      if Status_Updater = null then
         Status_Updater := new Update_Status;
      end if;

      Report_Handler.Add
        (Report_Data'(Key => +Key,
                      Project_Name => +Project_Name,
                      Scenario     => +Scenario,
                      Action       => +Action,
                      Output       => +Output,
                      Result       => Result,
                      Job_Id       => Job_Id,
                      Number       => <>));
   end Status;

   -------------------
   -- Update_Status --
   -------------------

   task body Update_Status is
      Last_Report : Report_Data;
   begin
      loop
         Report_Handler.Get (Last_Report);
         Logs.Write (-Last_Report.Key & ":" & (-Last_Report.Project_Name));
         Logs.Write ("Running Job Id" & Natural'Image (Last_Report.Job_Id)
                     & " :: " & (-Last_Report.Scenario)
                     & "/" & (-Last_Report.Action));
         Logs.Write ("Output is " & (-Last_Report.Output));
         Logs.Write (Boolean'Image (Last_Report.Result));

         if Last_Report.Action /= "" then
            --  This is the action log. Scenario is in progress
            Database.Log
              (-Last_Report.Key,
               -Last_Report.Project_Name,
               -Last_Report.Scenario,
               -Last_Report.Action,
               -Last_Report.Output,
               Last_Report.Result,
               Last_Report.Job_Id);

         else
            --  End of scenario. Final status
            Database.Final_Status (-Last_Report.Key,
                                   -Last_Report.Project_Name,
                                   -Last_Report.Scenario,
                                   Last_Report.Result,
                                   Last_Report.Job_Id);

            --  Send notification messages

            Savadur.Database.Send_Notifications
              (Project_Name   => -Last_Report.Project_Name,
               Send_Mail_Hook => Savadur.Notifications.Send_Mail'Access,
               Send_XMPP_Hook => Savadur.Notifications.XMPP_Send'Access,
               Subject        => "Running " & (-Last_Report.Project_Name),
               Content        => "End with "
               & Boolean'Image (Last_Report.Result)
               & " when running scenario " & (-Last_Report.Scenario));

            --  Update RSS file

            Notifications.Update_RSS;
         end if;
      end loop;
   end Update_Status;

end Savadur.Web_Services.Client;
