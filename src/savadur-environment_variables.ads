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

with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

package Savadur.Environment_Variables is

   use Ada;
   use Ada.Strings.Unbounded;

   type Var_Action is (Append, Clear, Replace);

   type Var is private;

   procedure Set_Value (V : in out Var; Value : in String);
   --  Set var value

   procedure Set_Action (V : in out Var; Action : in Var_Action);
   --  Set var action

   function Image (Variable : in Var) return String;
   --  Returns variable image

private
   type Var is record
      Value  : Unbounded_String;
      Action : Var_Action;
   end record;
end Savadur.Environment_Variables;
