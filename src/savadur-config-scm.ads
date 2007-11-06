
with Savadur.SCM;

package Savadur.Config.SCM is

   function Parse (SCM_Dir : in String) return Savadur.SCM.Maps.Map;
   --  Returns SCM config map

end Savadur.Config.SCM;
