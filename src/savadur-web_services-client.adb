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
with Ada.Exceptions;

with Morzhol.Logs;

with Savadur.Actions;
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

   use Ada.Exceptions;
   use Savadur.Utils;

   task type Update_Status;
   --  Asynchronous status updater

   Status_Updater : access Update_Status := null;

   type Report_Data (Start_Only : Boolean := False) is record
      Key          : Unbounded_String;
      Project_Name : Unbounded_String;
      Scenario     : Unbounded_String;
      Action       : Actions.Id;
      Job_Id       : Natural;
      Number       : Natural := 0;

      case Start_Only is
         when False =>
            Output       : Unbounded_String;
            Log_Filename : Unbounded_String;
            Result       : Boolean;
            Diff_Data    : Client.Diff_Data;

         when True =>
            null;
      end case;
   end record;

   function "<" (R1, R2 : in Report_Data) return Boolean;
   --  Returns True if R1 has been created before R2

   package Report_Set is
     new Containers.Ordered_Sets (Element_Type => Report_Data);

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

   function Load_Patch (Filename : in String) return File_Data is
   begin
      --  ??? Why using file_data as filename not used ?
      return File_Data'
        (Filename => +
           Filename,
         Content  => +Utils.Content
           (Directories.Compose (Config.Patch_Directory, Filename)));
   end Load_Patch;

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
         Load_Signed_File : declare
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
         end Load_Signed_File;

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
         Load_Signed_File : declare
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
         end Load_Signed_File;

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
      Logs.Write (Content => "Client Metadata are OS = " & (-Data.OS),
                  Kind    => Logs.Handler.Very_Verbose);

      Clients.Register
        (Key, Data, Clients.Idle, Server_Name, Callback_Endpoint);

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

   ------------------
   -- Status_Start --
   ------------------

   procedure Status_Start
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Job_Id       : in Natural)
   is
   begin
      if Status_Updater = null then
         Status_Updater := new Update_Status;
      end if;

      Report_Handler.Add
        (Report_Data'(Start_Only   => True,
                      Key          => +Key,
                      Project_Name => +Project_Name,
                      Scenario     => +Scenario,
                      Action       => Actions.Id_Utils.Value (Action),
                      Job_Id       => Job_Id,
                      Number       => <>));
   end Status_Start;

   ------------
   -- Status --
   ------------

   procedure Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Log_Filename : in String;
      Output       : in String;
      Result       : in Boolean;
      Job_Id       : in Natural;
      Diff_Data    : in Client.Diff_Data) is
   begin
      if Status_Updater = null then
         Status_Updater := new Update_Status;
      end if;

      Report_Handler.Add
        (Report_Data'(Start_Only   => False,
                      Key          => +Key,
                      Project_Name => +Project_Name,
                      Scenario     => +Scenario,
                      Action       => Actions.Id_Utils.Value (Action),
                      Log_Filename => +Log_Filename,
                      Output       => +Output,
                      Result       => Result,
                      Job_Id       => Job_Id,
                      Diff_Data    => Diff_Data,
                      Number       => <>));
   end Status;

   -------------------
   -- Update_Status --
   -------------------

   task body Update_Status is
      use type Actions.Id;
      Report : Report_Data;
   begin
      For_Every_Report : loop
         --  Wait for a new report data to be ready
         Report_Handler.Get (Report);

         if Report.Start_Only then
            Logs.Write (-Report.Key & ":" & (-Report.Project_Name));
            Logs.Write
              ("Running Job Id" & Natural'Image (Report.Job_Id)
               & " :: " & (-Report.Scenario)
               & "/" & (Actions.Id_Utils.To_String (Report.Action)));
            Database.Log_Start
              (Key          => -Report.Key,
               Project_Name => -Report.Project_Name,
               Scenario     => -Report.Scenario,
               Action       => Actions.Id_Utils.To_String (Report.Action),
               Job_Id       => Report.Job_Id);

         else
            Handle_Report : begin
               Logs.Write ("Output is " & (-Report.Output));
               Logs.Write (Boolean'Image (Report.Result));

               if Report.Action = Actions.End_Action.Id then
                  Clients.Set_Status (-Report.Key, Clients.Idle);

                  --  End of scenario. Final status

                  Database.Final_Status
                    (-Report.Key, -Report.Project_Name, -Report.Scenario,
                     Report.Result, Report.Job_Id);

                  --  Send notification messages

                  Savadur.Database.Send_Notifications
                    (Project_Name   => -Report.Project_Name,
                     Send_Mail_Hook => Savadur.Notifications.Send_Mail'Access,
                     Send_XMPP_Hook => Savadur.Notifications.XMPP_Send'Access,
                     Subject        => "Running " & (-Report.Project_Name),
                     Content        => "End with "
                     & Boolean'Image (Report.Result)
                     & " when running scenario " & (-Report.Scenario));

                  --  Update RSS file

                  Notifications.Update_RSS;

               else
                  Clients.Set_Status
                    (Key     => -Report.Key,
                     Status  => Clients.Busy,
                     Message => Actions.Id_Utils.To_String (Report.Action)
                     & " on " & (-Report.Project_Name)
                     & " (" & (-Report.Scenario) & ")");

                  --  This is the action log. Scenario is in progress
                  Database.Log
                    (-Report.Key, -Report.Project_Name, -Report.Scenario,
                     Actions.Id_Utils.To_String (Report.Action),
                     -Report.Log_Filename, -Report.Output,
                     Report.Result, Report.Job_Id);
               end if;

               if not Report.Result then
                  --  In all cases, if result is an error we send an e-amil to
                  --  all committers.

                  Send_Mails :
                  for K in Report.Diff_Data.Committers.Item'Range loop
                     Savadur.Notifications.Send_Mail
                       (Email   =>
                          To_String (Report.Diff_Data.Committers.Item (K)),
                        Subject => "Regression on project "
                        & To_String (Report.Project_Name),
                        Content =>  "Diff from "
                        & To_String (Report.Diff_Data.V1)
                        & " to " & To_String (Report.Diff_Data.V2) & ASCII.LF
                        & "when running scenario "
                        & To_String (Report.Scenario) & ASCII.LF);
                  end loop Send_Mails;
               end if;

            exception
               when E : others =>
                  Logs.Write
                    ("Update_Status exception : " & Exception_Information (E),
                     Kind => Morzhol.Logs.Error);
            end Handle_Report;
         end if;
      end loop For_Every_Report;
   end Update_Status;

end Savadur.Web_Services.Client;
