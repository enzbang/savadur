------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
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
with Ada.Strings.Hash_Case_Insensitive;

with Savadur.Action;
with Savadur.Scenario;
with Savadur.SCM;

package Savadur.Config.Project is

   use Ada;

   package Var_Maps is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   type Project_Config is record
      SCM      : Savadur.SCM.U_Id;
      Actions  : Savadur.Action.Maps.Map;
      Scenari  : Savadur.Scenario.Maps.Map;
      Variable : Var_Maps.Map;
   end record;

   function Parse (Filename : String) return Project_Config;

end Savadur.Config.Project;
