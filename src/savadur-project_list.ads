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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;

package Savadur.Project_List is

   use Ada;
   use Ada.Strings.Unbounded;

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

   package Projects is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Scenarios.Map,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => Scenarios."=");

   function Get_Clients (Project, Scenario : in String) return Clients.Vector;
   --  Returns the list of clients which can handle the give project/scenario

   function Image (Project_List : in Projects.Map) return String;
   --  Returns the Project_List map image

end Savadur.Project_List;
