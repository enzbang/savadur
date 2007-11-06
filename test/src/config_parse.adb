with Ada.Strings.Unbounded;

with Savadur.Config.Project;
with Savadur.Action;
with Savadur.Scenario;
package body Config_Parse is

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
   end Register_Tests;

end Config_Parse;
