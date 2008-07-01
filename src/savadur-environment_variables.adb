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

with Ada.Environment_Variables;
with Ada.Exceptions;

with Savadur.Logs;
with Savadur.Utils;

package body Savadur.Environment_Variables is

   use Savadur.Utils;

   -----------
   -- Image --
   -----------

   function Image (Variable : in Var) return String is
   begin
      return "value = " & (-Variable.Value) & ASCII.LF
        & "   action is " & Var_Action'Image (Variable.Action);
   end Image;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action (V : in out Var; Action : in Var_Action) is
   begin
      V.Action := Action;
   end Set_Action;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (V : in out Var; Value : in String) is
   begin
      V.Value := +Value;
   end Set_Value;

end Savadur.Environment_Variables;
