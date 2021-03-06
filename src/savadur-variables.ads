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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Sets;

limited with Savadur.Projects;
with Savadur.Utils;

package Savadur.Variables is

   use Ada;
   use Ada.Strings.Unbounded;

   type Name is new Unbounded_String;

   package Name_Utils is new Utils.Generic_Utils (Source => Name);

   type Variable is record
      Name  : Variables.Name;
      Value : Unbounded_String;
   end record;

   function Image (Var : in Variable) return String;
   --  Returns var image

   ----------
   -- Sets --
   ----------

   function Hash (Key : in Variable) return Containers.Hash_Type;

   package Sets is new Containers.Indefinite_Hashed_Sets
     (Element_Type        => Variable,
      Hash                => Hash,
      Equivalent_Elements => "=");

   function Key (Element : in Variable) return Name;
   --  Returns variable name

   function Hash (Key : in Name) return Containers.Hash_Type;

   package Keys is new Sets.Generic_Keys
     (Key_Type        => Name,
      Key             => Key,
      Hash            => Hash,
      Equivalent_Keys => "=");

   procedure Default (Project : access Projects.Project_Config);
   --  Sets default variables :
   --     - sources is set as "sources"
   --     - project_dir

   function Image (Set : in Sets.Set) return String;
   --  Returns map image

end Savadur.Variables;
