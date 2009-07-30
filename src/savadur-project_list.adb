------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Text_IO;

with Savadur.Config.Project;
with Savadur.Config.Project_List;

with Savadur.Jobs.Server;
with Savadur.Projects.Sets;
with Savadur.Scenarios;
with Savadur.Utils;

package body Savadur.Project_List is

   use Savadur.Utils;

   package Client_Set is new Containers.Indefinite_Hashed_Sets
     (String, Strings.Hash, "=");

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Project) return Boolean is
   begin
      return Scenarios."=" (Left.S_Map, Right.S_Map)
        and then Left.Log_Size = Right.Log_Size;
   end "=";

   function Force_Validity_Check
     (Project_List : in Projects.Map) return Boolean is

      Is_Valid : Boolean := True;

      procedure Check_Project (Position : in Projects.Cursor);
      --  Check if the project scenarios are defined in the corresponding
      --  project description

      -------------------
      -- Check_Project --
      -------------------

      procedure Check_Project (Position : in Projects.Cursor) is
         Name   : constant String := Projects.Key (Position);
         Config : constant Savadur.Projects.Project_Config
           := Savadur.Config.Project.Get (Name);
         Project_Scenarios : constant Scenarios.Map :=
                               Projects.Element (Position).S_Map;
         Scenario_Position : Scenarios.Cursor := Project_Scenarios.First;
      begin
         For_All : while Scenarios.Has_Element (Scenario_Position) loop
            Check_Scenario : declare
               Current_Scenario : constant String :=
                                    Scenarios.Key (Scenario_Position);
               Scenario_Id      : constant Savadur.Scenarios.Id :=
                                    Savadur.Scenarios.Id_Utils.Value
                                      (Current_Scenario);
            begin
               if not Savadur.Scenarios.Keys.Contains
                 (Container => Config.Scenarios,
                  Key       => Scenario_Id) then
                  Is_Valid := False;
                  Text_IO.Put_Line ("Error: unknown scenario "
                                    & Current_Scenario & " in project "
                                    & Name);
               end if;
            end Check_Scenario;
            Scenarios.Next (Scenario_Position);
         end loop For_All;
      end Check_Project;

   begin

      Project_List.Iterate (Check_Project'Access);
      return Is_Valid;
   end Force_Validity_Check;

   ----------------
   -- Get_Client --
   ----------------

   function Get_Client
     (Project, Client_Name : in String) return Client
   is
      P_Position : constant Projects.Cursor :=
                     Config.Project_List.Configurations.Find (Project);
   begin
      if Projects.Has_Element (P_Position) then
         Get_Registered_Clients : declare
            Scenarios  : constant Project_List.Scenarios.Map :=
                           Projects.Element (P_Position).S_Map;
            S_Position : constant Project_List.Scenarios.Cursor :=
                           Scenarios.First;
         begin
            if Project_List.Scenarios.Has_Element (S_Position) then
               declare
                  Client_List : constant Clients.Vector :=
                                  Project_List.Scenarios.Element (S_Position);
                  C_Position  : Clients.Cursor := Clients.First (Client_List);
                  C           : Client;
               begin
                  while Clients.Has_Element (C_Position) loop
                     C := Clients.Element (C_Position);

                     if -C.Key = Client_Name then
                        return C;
                     end if;

                     Clients.Next (C_Position);
                  end loop;
               end;
            end if;
         end Get_Registered_Clients;
      end if;

      return No_Data;
   end Get_Client;

   -----------------
   -- Get_Clients --
   -----------------

   function Get_Clients
     (Project, Scenario : in String) return Clients.Vector
   is
      P_Position : constant Projects.Cursor :=
                     Config.Project_List.Configurations.Find (Project);
      Result     : Clients.Vector;
   begin
      if Projects.Has_Element (P_Position) then
         Get_Registered_Clients : declare
            Scenarios  : constant Project_List.Scenarios.Map :=
                           Projects.Element (P_Position).S_Map;
            S_Position : constant Project_List.Scenarios.Cursor :=
                           Scenarios.Find (Scenario);
         begin
            if Project_List.Scenarios.Has_Element (S_Position) then
               Result := Project_List.Scenarios.Element (S_Position);
            end if;
         end Get_Registered_Clients;
      end if;

      return Result;
   end Get_Clients;

   ------------------
   -- Get_Log_Size --
   ------------------

   function Get_Log_Size (Project : in String) return Natural is
      P_Position : constant Projects.Cursor :=
                     Config.Project_List.Configurations.Find (Project);
   begin
      if Projects.Has_Element (P_Position) then
         return Projects.Element (P_Position).Log_Size;
      else
         return Default_Log_Size;
      end if;
   end Get_Log_Size;

   -----------
   -- Image --
   -----------

   function Image (Project_List : in Projects.Map) return String is

      procedure Image_Scenarios (Position : in Scenarios.Cursor);

      procedure Image_Clients (Position : in Clients.Cursor);

      procedure Image_Projects (Position : in Projects.Cursor);

      Result : Unbounded_String;

      -------------------
      -- Image_Clients --
      -------------------

      procedure Image_Clients (Position : in Clients.Cursor) is
      begin
         Result := Result
           & "      . " & Clients.Element (Position).Key & ASCII.LF;
      end Image_Clients;

      --------------------
      -- Image_Projects --
      --------------------

      procedure Image_Projects (Position : in Projects.Cursor) is
      begin
         Result := Result & "* " & Projects.Key (Position) & ASCII.LF;

         Projects.Element (Position).S_Map.Iterate (Image_Scenarios'Access);
      end Image_Projects;

      ---------------------
      -- Image_Scenarios --
      ---------------------

      procedure Image_Scenarios (Position : in Scenarios.Cursor) is
      begin
         Result := Result & "   - " & Scenarios.Key (Position) & ASCII.LF;

         Result := Result & "[" & ASCII.LF;
         Scenarios.Element (Position).Iterate (Image_Clients'Access);
         Result := Result & "]";
      end Image_Scenarios;

   begin
      Project_List.Iterate (Image_Projects'Access);

      return To_String (Result);
   end Image;

   ------------------------
   -- Iterate_On_Clients --
   ------------------------

   procedure Iterate_On_Clients
     (Project_List : in Projects.Map;
      Action       : access procedure (Client : in String))
   is
      Position : Projects.Cursor := Project_List.First;
      Set      : Client_Set.Set;
   begin
      For_All_Projects : while Projects.Has_Element (Position) loop
         Get_Scenarios_List : declare
            M_Scerarios        : constant Scenarios.Map :=
                                   Projects.Element (Position).S_Map;
            Scenarios_Position : Scenarios.Cursor := M_Scerarios.First;
         begin

            --  For all projects scenarios

            For_All_Scenarios :
            while Scenarios.Has_Element (Scenarios_Position) loop

               Get_Projects_List : declare
                  V_Clients          : constant Clients.Vector :=
                                          Scenarios.Element
                                              (Scenarios_Position);
                  Clients_Position   : Clients.Cursor := V_Clients.First;
               begin

                  --  For all clients registered for this scenario

                  For_All_Clients :
                  while Clients.Has_Element (Clients_Position) loop
                     declare
                        Client_Key : constant String :=
                                       -Clients.Element (Clients_Position).Key;
                     begin
                        if not Set.Contains (Client_Key) then
                           Action (Client_Key);
                           Set.Insert (Client_Key);
                        end if;
                        Clients.Next (Clients_Position);
                     end;
                  end loop For_All_Clients;
               end Get_Projects_List;

               Scenarios.Next (Scenarios_Position);
            end loop For_All_Scenarios;
         end Get_Scenarios_List;

         Projects.Next (Position);
      end loop For_All_Projects;
   end Iterate_On_Clients;

   ------------
   -- To_Set --
   ------------

   function To_Set
     (Project_List : in Projects.Map) return AWS.Templates.Translate_Set
   is
      use AWS.Templates;

      Result      : AWS.Templates.Translate_Set;

      T_Clients   : AWS.Templates.Tag;
      T_Projects  : AWS.Templates.Tag;
      T_Next_Run  : AWS.Templates.Tag;
      T_Scenarios : AWS.Templates.Tag;

      Position : Projects.Cursor := Project_List.First;

   begin

      For_All_Projects : while Projects.Has_Element (Position) loop
         Get_Scenarios_List : declare
            M_Scerarios                : constant Scenarios.Map :=
                                            Projects.Element (Position).S_Map;
            Scenarios_Position         : Scenarios.Cursor       :=
                                            M_Scerarios.First;
            T_Project_Scenarios        : Tag;
            T_Project_Scenario_Clients : Tag;
         begin
            --  For all projects scenarios

            For_All_Scenarios :
            while Scenarios.Has_Element (Scenarios_Position) loop
               Get_Projects_List : declare
                  T_Scenarios_Client : Tag;
                  V_Clients          : constant Clients.Vector :=
                                         Scenarios.Element
                                           (Scenarios_Position);
                  Clients_Position   : Clients.Cursor := V_Clients.First;
               begin
                  --  For all clients registered for this scenario

                  For_All_Clients :
                  while Clients.Has_Element (Clients_Position) loop
                     T_Scenarios_Client := T_Scenarios_Client
                       & Clients.Element (Clients_Position).Key;

                     Clients.Next (Clients_Position);
                  end loop For_All_Clients;
                  T_Project_Scenarios := T_Project_Scenarios
                    & Scenarios.Key (Scenarios_Position);
                  T_Project_Scenario_Clients := T_Project_Scenario_Clients
                    & T_Scenarios_Client;
               end Get_Projects_List;

               Scenarios.Next (Scenarios_Position);
            end loop For_All_Scenarios;

            declare
               Project_Key : constant String := Projects.Key (Position);
               Position    : constant Savadur.Projects.Sets.Sets.Cursor :=
                               Config.Project.Configurations.Find
                                 (Savadur.Projects.Project_Config'
                                    (Project_Id =>
                                       Savadur.Projects.Id_Utils.Value
                                         (Project_Key),
                                     others     => <>));
               Project     : constant Savadur.Projects.Project_Config :=
                               Savadur.Projects.Sets.Sets.Element (Position);
               D           : Duration;
            begin
               T_Projects := T_Projects & Project_Key;

               D := Jobs.Server.Queue.Next_Job_In (Project.Signature);

               if D = 0.0 then
                  T_Next_Run := T_Next_Run & "";

               else
                  if D > 60.0 then
                     T_Next_Run := T_Next_Run
                       & (Integer'Image (Integer (D / 60.0)) & " mins");
                  else
                     T_Next_Run := T_Next_Run
                       & (Integer'Image (Integer (D)) & " secs");
                  end if;
               end if;
            end;

            T_Scenarios := T_Scenarios & T_Project_Scenarios;
            T_Clients   := T_Clients & T_Project_Scenario_Clients;

         end Get_Scenarios_List;

         Projects.Next (Position);
      end loop For_All_Projects;

      AWS.Templates.Insert (Set  => Result,
                            Item => AWS.Templates.Assoc
                              (Variable  => "PROJECTS",
                               Value     => T_Projects));

      AWS.Templates.Insert (Set  => Result,
                            Item => AWS.Templates.Assoc
                              (Variable  => "PROJECTS_NEXT_RUN",
                               Value     => T_Next_Run));

      AWS.Templates.Insert (Set  => Result,
                            Item => AWS.Templates.Assoc
                              (Variable  => "SCENARIOS",
                               Value     => T_Scenarios));

      AWS.Templates.Insert (Set  => Result,
                            Item => AWS.Templates.Assoc
                              (Variable  => "CLIENTS",
                               Value     => T_Clients));

      return Result;
   end To_Set;

end Savadur.Project_List;
