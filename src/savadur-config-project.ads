
with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

package Savadur.Config.Project is

   type Project_Config is record
      SCM     : Savadur.SCM.U_Id;
      Actions : Savadur.Action.Maps.Map;
      Scenari : Savadur.Scenario.Maps.Map;
   end record;

   function Parse (Filename : String) return Project_Config;

end Savadur.Config.Project;
