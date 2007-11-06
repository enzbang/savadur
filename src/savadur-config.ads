
with Ada.Strings.Unbounded;
with Savadur.Action;
with Savadur.Scenario;

package Savadur.Config is

   use Ada.Strings.Unbounded;

   type Project_Config is record
      SCM : Unbounded_String;
      Actions : Savadur.Action.Maps.Map;
      Scenari : Savadur.Scenario.Maps.Map;
   end record;

   function Parse (Filename : String) return Project_Config;

end Savadur.Config;
