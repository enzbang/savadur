
with Ada.Strings.Unbounded;

with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

package Savadur.Config.SCM is

   use Ada.Strings.Unbounded;

   function Parse (SCM_Dir : in String) return Savadur.SCM.Maps.Map;
   --  Returns SCM config map

end Savadur.Config.SCM;
