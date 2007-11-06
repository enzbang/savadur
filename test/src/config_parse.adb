with Ada.Strings.Unbounded;

with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

package body Config_Parse is

   procedure Check_SCM_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Check_Project_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   --------------------------
   -- Check_Project_Config --
   --------------------------

   procedure Check_Project_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
     use Ada.Strings.Unbounded;

      Project : Savadur.Config.Project.Project_Config :=
                  Savadur.Config.Project.Parse ("ex_project.xml");
   begin

      Assertions.Assert
        (To_String (Unbounded_String (Project.SCM)) = "git",
         "SCM Name error");

      Assertions.Assert
        (Savadur.Action.Image (Project.Actions) = "[" & ASCII.Lf
           & "make => make build" & ASCII.Lf & "]",
         "Wrong action list");

      Assertions.Assert
        (Savadur.Scenario.Image (Project.Scenari) = "* default" & ASCII.Lf
         & "Mode : on-change" & ASCII.Lf
         & "[" & ASCII.Lf
         & "SCM update" & ASCII.Lf
         & "DEFAULT make" & ASCII.Lf
         & "]",
         "Wrong scenari list");
   end Check_Project_Config;

   ----------------------
   -- Check_SCM_Config --
   ----------------------

   procedure Check_SCM_Config
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
     use Ada.Strings.Unbounded;

      SCM_Map : Savadur.SCM.Maps.Map :=
                  Savadur.Config.SCM.Parse ("../config/scm");
   begin
      Assertions.Assert
        (Savadur.SCM.Image (SCM_Map) = "* git" & ASCII.Lf
         & "[" & ASCII.Lf
         & "init => git-clone $URL $SOURCES_DIR"  & ASCII.Lf
         & "pull => git-pull"  & ASCII.Lf
         & "version => git-show-ref -s"  & ASCII.Lf
         & "]",
         "Wrong SCM parse");
   end Check_SCM_Config;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return Test_String is
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

   end Register_Tests;

end Config_Parse;
