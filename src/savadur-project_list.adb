------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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

with Savadur.Config.Project_List;
with Savadur.Utils;

package body Savadur.Project_List is

   use Savadur.Utils;

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
         declare
            Scenarios  : constant Project_List.Scenarios.Map :=
                           Projects.Element (P_Position);
            S_Position : constant Project_List.Scenarios.Cursor :=
                           Scenarios.Find (Scenario);
         begin
            if Project_List.Scenarios.Has_Element (S_Position) then
               Result := Project_List.Scenarios.Element (S_Position);
            end if;
         end;
      end if;

      return Result;
   end Get_Clients;

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

         Projects.Element (Position).Iterate (Image_Scenarios'Access);
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
     (Project_List : in Projects.Map; Action : in Iterate_Action) is
      Position : Projects.Cursor := Project_List.First;
   begin
      --  For all projects

      while Projects.Has_Element (Position) loop
         Get_Scenarios_List : declare
            M_Scerarios                : constant Scenarios.Map :=
                                            Projects.Element (Position);
            Scenarios_Position         : Scenarios.Cursor       :=
                                            M_Scerarios.First;
         begin
            --  For all projects scenarios

            while Scenarios.Has_Element (Scenarios_Position) loop

               Get_Projects_List : declare
                  V_Clients          : constant Clients.Vector :=
                                          Scenarios.Element
                                              (Scenarios_Position);
                  Clients_Position   : Clients.Cursor := V_Clients.First;
               begin

                  --  For all clients registered for this scenario

                  while Clients.Has_Element (Clients_Position) loop
                     Action.all (-Clients.Element (Clients_Position).Key);
                     Clients.Next (Clients_Position);
                  end loop;
               end Get_Projects_List;

               Scenarios.Next (Scenarios_Position);
            end loop;
         end Get_Scenarios_List;

         Projects.Next (Position);
      end loop;
   end Iterate_On_Clients;

   ------------
   -- To_Set --
   ------------

   function To_Set
     (Project_List : in Projects.Map) return AWS.Templates.Translate_Set is
      use AWS.Templates;

      Result      : AWS.Templates.Translate_Set;

      T_Clients   : AWS.Templates.Tag;
      T_Projects  : AWS.Templates.Tag;
      T_Scenarios : AWS.Templates.Tag;

      Position : Projects.Cursor := Project_List.First;

   begin

      --  For all projects

      while Projects.Has_Element (Position) loop
         Get_Scenarios_List : declare
            T_Project_Scenarios        : Tag;
            T_Project_Scenario_Clients : Tag;
            M_Scerarios                : constant Scenarios.Map :=
                                            Projects.Element (Position);
            Scenarios_Position         : Scenarios.Cursor       :=
                                            M_Scerarios.First;

         begin
            --  For all projects scenarios

            while Scenarios.Has_Element (Scenarios_Position) loop
               Get_Projects_List : declare
                  T_Scenarios_Client : Tag;
                  V_Clients          : constant Clients.Vector :=
                                         Scenarios.Element
                                           (Scenarios_Position);
                  Clients_Position   : Clients.Cursor := V_Clients.First;
               begin

                  --  For all clients registered for this scenario

                  while Clients.Has_Element (Clients_Position) loop
                     T_Scenarios_Client := T_Scenarios_Client
                       & Clients.Element (Clients_Position).Key;

                     Clients.Next (Clients_Position);
                  end loop;
                  T_Project_Scenarios := T_Project_Scenarios
                    & Scenarios.Key (Scenarios_Position);
                  T_Project_Scenario_Clients := T_Project_Scenario_Clients
                    & T_Scenarios_Client;
               end Get_Projects_List;

               Scenarios.Next (Scenarios_Position);
            end loop;

            T_Projects  := T_Projects & Projects.Key (Position);
            T_Scenarios := T_Scenarios & T_Project_Scenarios;
            T_Clients   := T_Clients & T_Project_Scenario_Clients;

         end Get_Scenarios_List;

         Projects.Next (Position);
      end loop;

      AWS.Templates.Insert (Set  => Result,
                            Item => AWS.Templates.Assoc
                              (Variable  => "PROJECTS",
                               Value     => T_Projects));

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
