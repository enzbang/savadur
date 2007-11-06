
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

procedure Savadur.Client is
   Project : Savadur.Config.Project.Project_Config :=
               Savadur.Config.Project.Parse ("test/savadurrc");

   SCM_Map : Savadur.SCM.Maps.Map := Savadur.Config.SCM.Parse ("config/scm");

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
begin

   Put_Line ("Savadur client");
   New_Line;
   Put_Line ("SCM : " & To_String (Unbounded_String (Project.SCM)));
   New_Line;
   Put_Line ("Action list : ");
   New_Line;
   Put_Line (Savadur.Action.Image (Project.Actions));
   New_Line;
   Put_Line ("Scenari : ");
   New_Line;
   Put_Line (Savadur.Scenario.Image (Project.Scenari));
   New_Line;
   New_Line;
   New_Line;
   Put_Line ("SCM Found");
   New_Line;
   Put_Line (Savadur.SCM.Image (SCM_Map));

end Savadur.Client;
