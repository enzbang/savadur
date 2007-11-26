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

with Savadur.Config.Project_List;

package body Savadur.Project_List is

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

end Savadur.Project_List;
