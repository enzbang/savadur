------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
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

with Savadur.Utils;

package body Savadur.Project_List is

   use Savadur.Utils;

   Project_Mapping : Projects.Map;

   -----------------
   -- Get_Clients --
   -----------------

   function Get_Clients
     (Project, Scenario : in String) return Clients.Vector
   is
      P_Position : constant Projects.Cursor := Project_Mapping.Find (Project);
      Result     : Clients.Vector;
   begin
      if Projects.Has_Element (P_Position) then
         declare
            S_Position : constant Scenarios.Cursor :=
                           Projects.Element (P_Position).Find (Scenario);
         begin
            if Scenarios.Has_Element (S_Position) then
               Result := Scenarios.Element (S_Position);
            end if;
         end;
      end if;

      return Result;
   end Get_Clients;

   ---------------------
   -- Register_Client --
   ---------------------

   procedure Register_Client
     (Project  : in String;
      Scenario : in String;
      Client   : in String)
   is
      procedure Update_Scenario
        (Key     : in String;
         Element : in out Scenarios.Map);

      procedure Update_Client
        (Key     : in String;
         Element : in out Clients.Vector);

      -------------------
      -- Update_Client --
      -------------------

      procedure Update_Client
        (Key     : in String;
         Element : in out Clients.Vector)
      is
         pragma Unreferenced (Key);
      begin
         Element.Append (New_Item => (+Client, True));
      end Update_Client;

      ---------------------
      -- Update_Scenario --
      ---------------------

      procedure Update_Scenario
        (Key     : in String;
         Element : in out Scenarios.Map)
      is
         pragma Unreferenced (Key);
         Position : constant Scenarios.Cursor := Element.Find (Scenario);
      begin
         if not Scenarios.Has_Element (Position) then
            declare
               V : Clients.Vector;
            begin
               Element.Insert (Scenario, New_Item => V);
            end;
         end if;

         Element.Update_Element (Position, Update_Client'Access);
      end Update_Scenario;

      Position : constant Projects.Cursor := Project_Mapping.Find (Project);

   begin
      if not Projects.Has_Element (Position) then
         declare
            M : Scenarios.Map;
         begin
            Project_Mapping.Insert (Project, New_Item => M);
         end;
      end if;

      Project_Mapping.Update_Element (Position, Update_Scenario'Access);
   end Register_Client;

end Savadur.Project_List;
