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

with Ada.Strings.Unbounded;
with Ada.Directories;

with AWS.Jabber;

with Savadur.Actions;
with Savadur.Config.Environment_Variables;
with Savadur.Config.Notifications.XMPP;
with Savadur.Config.Notifications.SMTP;
with Savadur.Config.Project;
with Savadur.Config.Project_List;
with Savadur.Config.SCM;
with Savadur.Config.Server;
with Savadur.Config;
with Savadur.Environment_Variables;
with Savadur.Notifications;
with Savadur.Projects;
with Savadur.Project_List;
with Savadur.Scenarios;
with Savadur.SCM;
with Savadur.Servers;
with Savadur.Variables;
with Savadur.Utils;

with Utils;

package body Config_Parse is

   use Ada;
   use Savadur;
   use Savadur.Utils;

   Project : aliased Savadur.Projects.Project_Config;

   procedure Check_SCM_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Project_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Env_Var_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Server_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Project_List
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Notify_XMPP
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Notify_SMTP
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   --------------------------
   -- Check_Env_Var_Config --
   --------------------------

   procedure Check_Env_Var_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Strings.Unbounded;

      Env_Var : Savadur.Environment_Variables.Maps.Map :=
                  Savadur.Config.
                    Environment_Variables.Parse (Project'Access);
   begin
      Assertions.Assert
        (Utils.Strip (Savadur.Environment_Variables.Image (Env_Var)) =
           Utils.Strip ("["
         & "LD_LIBRARY_PATH : value = /opt/lib"
         & "action is REPLACE"
         & "PATH : value = /usr/bin"
         & "action is APPEND"
         & "SAVADUR_DIR : value = "
         & "action is CLEAR"
         & "]"),
         "Wrong env variable list");
   end Check_Env_Var_Config;

   -----------------------
   -- Check_Notify_SMTP --
   -----------------------

   procedure Check_Notify_SMTP
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use type AWS.Jabber.Authentication_Type;
   begin
      Config.Notifications.SMTP.Parse;
      Assertions.Assert
        (Config.Notifications.SMTP.Server = "smtp.example.com",
         "Wrong SMTP server");

      Assertions.Assert
        (Config.Notifications.SMTP.User = "savadur@here.com",
        "Wrong SMTP User");

      Assertions.Assert
        (Config.Notifications.SMTP.Password = "secret_smtp_pwd",
         "Wrong SMTP password");
   end Check_Notify_SMTP;

   -----------------------
   -- Check_Notify_XMPP --
   -----------------------

   procedure Check_Notify_XMPP
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use type AWS.Jabber.Authentication_Type;
   begin
      Config.Notifications.XMPP.Parse;
      Assertions.Assert
        (Config.Notifications.XMPP.Jabber_Server = "server.example.com",
         "Wrong jabber server");

      Assertions.Assert
        (Config.Notifications.XMPP.Jabber_JID = "savadur",
        "Wrong jabber JID");

      Assertions.Assert
        (Config.Notifications.XMPP.Jabber_Password = "myPaSsW0rd",
         "Wrong jabber password");

      Assertions.Assert
        (Config.Notifications.XMPP.Jabber_Auth_Type = AWS.Jabber.PLAIN,
        "Wrong jabber authentication type");
   end Check_Notify_XMPP;

   --------------------------
   -- Check_Project_Config --
   --------------------------

   procedure Check_Project_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Strings.Unbounded;
   begin
      Assertions.Assert
        (-Unbounded_String (Project.Project_Id) = "regtests",
         "Project Name error");

      Assertions.Assert
        (-Unbounded_String (Project.Description) = "An in-depth look at "
         & "creating applications with XML, using <, >,",
         "Project description error");

      Assertions.Assert
        (-Unbounded_String (Project.SCM_Id) = "git",
         "SCM Name error");

      Assertions.Assert
        (Utils.Strip (Savadur.Actions.Image (Project.Actions)) =
           Utils.Strip ("["
             & "mail => /hooks/send-mail $project_name $failed_action"
             & "  result type : EXIT_STATUS"
             & "make => make build  result type : EXIT_STATUS"
             & "jabber => /hooks/send-xmpp $project_name"
             & "  result type : EXIT_STATUS"
             & "regtests => make regtests  result type : EXIT_STATUS"
             & "]"),
         "Wrong action list");

      Assertions.Assert
        (Utils.Strip (Savadur.Variables.Image (Project.Variables)) =
           Utils.Strip ("["
             & "project_name : regtests"
             & "project_dir : "
             & Directories.Compose
               (Containing_Directory => Savadur.Config.Work_Directory,
                Name                 => "regtests")
             & "url : ../../../../"
             & "sources : sources"
             & "]"),
         "Wrong variable list");

      Assertions.Assert
        (Utils.Strip (Savadur.Scenarios.Image (Project.Scenarios)) =
           Utils.Strip ("* default"
         & "["
         & "SCM version require_change on error = QUIT"
         & "SCM pull"
         & "DEFAULT make"
         & "DEFAULT regtests"
         & "] periodic = 23:30/+60"),
         "Wrong scenarios list");

      Assertions.Assert
        (Utils.Strip (Savadur.Notifications.Image (Project.Notifications)) =
           Utils.Strip ("Notification :"
             & "On Success = ["
             & "DEFAULT jabber"
             & "]"
             & "On Failure = ["
             & "DEFAULT mail"
             & "]"),
         "Wrong notifications list");
   end Check_Project_Config;

   ------------------------
   -- Check_Project_List --
   ------------------------

   procedure Check_Project_List
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Savadur.Config.Project_List.Parse;
      Assertions.Assert
        (Utils.Strip (Savadur.Project_List.Image
         (Savadur.Config.Project_List.Configurations)) =
           Utils.Strip ("* style_checker"
             & " - nightly"
             & "["
             & " . azerty"
             & "]"
             & " - daily"
             & "["
             & " . qwerty"
             & "]"
             & " * dummy"
             & " - donotcare"
             & "["
             & " . me"
             & " . and_just_me"
             & "]"),
         "Wrong Project_List parse" & Savadur.Project_List.Image
           (Savadur.Config.Project_List.Configurations));
   end Check_Project_List;

   ----------------------
   -- Check_SCM_Config --
   ----------------------

   procedure Check_SCM_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Strings.Unbounded;
   begin
      Savadur.Config.SCM.Parse;
      Assertions.Assert
        (Utils.Strip (Savadur.SCM.Image
         (Savadur.Config.SCM.Configurations)) =
           Utils.Strip ("* git"
         & "["
         & "init => git-clone $url $sources  result type : EXIT_STATUS"
         & "pull => git-pull  result type : EXIT_STATUS"
         & "version => git-show-ref -s refs/heads/master  "
         & "result type : VALUE"
         & "]"
         & "* subversion"
         & "["
         & "update => svn update  result type : EXIT_STATUS"
         & "init => svn checkout $url $sources  result type : EXIT_STATUS"
         & "version => svn info (Revision: (.*))  result type : VALUE"
         & "]"),
         "Wrong SCM parse" & Savadur.SCM.Image
         (Savadur.Config.SCM.Configurations));
   end Check_SCM_Config;

   -------------------------
   -- Check_Server_Config --
   -------------------------

   procedure Check_Server_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Strings.Unbounded;
   begin
      Savadur.Config.Set_Savadur_Directory ("config");

      Savadur.Config.Server.Parse;
      Assertions.Assert
        (Savadur.Servers.Image =
           "* a_fast_server" & ASCII.LF
         & "[" & ASCII.LF
         & "Name => a_fast_server"  & ASCII.LF
         & "URL => localhost"  & ASCII.LF
         & "]" & ASCII.LF
         & "* myserver" & ASCII.LF
         & "[" & ASCII.LF
         & "Name => myserver"  & ASCII.LF
         & "URL => http://www.myserver.net/" & ASCII.LF
         & "]" & ASCII.LF
         & "* test_server" & ASCII.LF
         & "[" & ASCII.LF
         & "Name => test_server"  & ASCII.LF
         & "URL => http://localhost:8181" & ASCII.LF
         & "]" & ASCII.LF,
         "Wrong Servers parse, expected : " & ASCII.LF
         & ''' & Savadur.Servers.Image & ''');
   end Check_Server_Config;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Parse configuration files and check generated results");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Check_Project_Config'Access, "check project configuration");
      Registration.Register_Routine
        (T, Check_SCM_Config'Access, "check scm configuration");
      Registration.Register_Routine
        (T, Check_Env_Var_Config'Access, "check env var configuration");
      Registration.Register_Routine
        (T, Check_Server_Config'Access, "check server configuration");
      Registration.Register_Routine
        (T, Check_Project_List'Access, "check project list");
      Registration.Register_Routine
        (T, Check_Notify_XMPP'Access, "check XMPP notify configuration");
      Registration.Register_Routine
        (T, Check_Notify_SMTP'Access, "check SMTP notify configuration");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   procedure Set_Up_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      Savadur.Config.Project.Parse;
      Project := Savadur.Config.Project.Get (Project_Name => "regtests");
   end Set_Up_Case;

end Config_Parse;
