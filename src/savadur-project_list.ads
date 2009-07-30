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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;

with AWS.Templates;

package Savadur.Project_List is

   use Ada;
   use Ada.Strings.Unbounded;

   Default_Log_Size : constant := 100;

   --  Client set

   type Client is record
      Key       : Unbounded_String;
      Activated : Boolean := True;
   end record;

   package Clients is new Containers.Indefinite_Vectors
     (Index_Type      => Positive,
      Element_Type    => Client);

   --  For each scenario

   package Scenarios is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Clients.Vector,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => Clients."=");

   --  For each projects

   type Project is record
      S_Map    : Scenarios.Map;
      Log_Size : Natural;
      --  Max log size to display, 0 means no limit
   end record;

   function "=" (Left, Right : in Project) return Boolean;

   package Projects is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Project,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   function Get_Clients (Project, Scenario : in String) return Clients.Vector;
   --  Returns the list of clients which can handle the give project/scenario

   function Get_Log_Size (Project : in String) return Natural;
   --  Returns the log size limit for the given project

   function Image (Project_List : in Projects.Map) return String;
   --  Returns the Project_List map image

   function To_Set
     (Project_List : in Projects.Map) return AWS.Templates.Translate_Set;
   --  Returns a translate set

   procedure Iterate_On_Clients
     (Project_List : in Projects.Map;
      Action       : access procedure (Client : in String));
   --  Iterate the given action on all clients

   function Force_Validity_Check
     (Project_List : in Projects.Map) return Boolean;
   --  Check that all scenarios, projects defined in project list are defined
   --  Returns False if a scenario is undefined.

end Savadur.Project_List;
