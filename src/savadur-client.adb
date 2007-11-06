
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Savadur.Config;
with Savadur.Action;
with Savadur.Scenario;

procedure Savadur.Client is
   Project : Savadur.Config.Project_Config :=
               Savadur.Config.Parse ("test/savadurrc");

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
end Savadur.Client;
