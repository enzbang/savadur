------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
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

with Savadur.Utils;
with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Actions;
with Savadur.Scenarios;
with Savadur.Variables;
with Savadur.SCM;
with Savadur.Environment_Variables;
with Savadur.Config.Environment_Variables;
with Savadur.Notifications;

with Utils;
with Savadur.Config; use Savadur.Config;

package body Config_Parse is

   use Ada;
   use Savadur.Utils;

   procedure Check_SCM_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Project_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Env_Var_Config
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
                    Environment_Variables.Parse ("ex_project_env.xml");
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

   --------------------------
   -- Check_Project_Config --
   --------------------------

   procedure Check_Project_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Strings.Unbounded;

      Project : Savadur.Config.Project.Project_Config :=
                  Savadur.Config.Project.Parse (Project_Name => "ex_project",
                                                Filename => "ex_project.xml");
   begin
      Assertions.Assert
        (-Unbounded_String (Project.Project_Id) = "ex_project",
         "Project Name error");

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
             & "project_dir : "
             & Directories.Compose
               (Containing_Directory => Savadur.Config.Work_Directory,
                Name                 => "ex_project")
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
         & "]"),
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
         "Wrong notifications list" &
         Savadur.Notifications.Image (Project.Notifications));

   end Check_Project_Config;

   ----------------------
   -- Check_SCM_Config --
   ----------------------

   procedure Check_SCM_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Strings.Unbounded;
   begin
      Savadur.Config.Set_Savadur_Directory ("../config");

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
         & "]"),
         "Wrong SCM parse");
   end Check_SCM_Config;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case) return Test_String is
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

   end Register_Tests;

end Config_Parse;
