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

   -----------
   -- Image --
   -----------

   function Image (Map : in Maps.Map) return String is
      Position : Maps.Cursor := Maps.First (Map);
      Result   : Unbounded_String := +"[" & ASCII.Lf;
   begin
      while Maps.Has_Element (Position) loop
         Append (Result, Maps.Key (Position) & " : "
                 & Image (Maps.Element (Position)) & ASCII.Lf);
         Maps.Next (Position);
      end loop;
      return -Result & "]";
   end Image;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment (Map : in Maps.Map) is
      Position : Maps.Cursor := Map.First;
   begin
      while Maps.Has_Element (Position) loop
         Set_Var : declare
            V : constant Var    := Maps.Element (Position);
            N : constant String := Maps.Key (Position);
         begin
            case V.Action is
               when Replace =>
                  Ada.Environment_Variables.Set (N, -V.Value);

               when Append  =>
                  if Ada.Environment_Variables.Exists (N) then
                     Ada.Environment_Variables.Set
                       (N,
                        -V.Value & ":" & Ada.Environment_Variables.Value (N));
                  else
                     Ada.Environment_Variables.Set (N, -V.Value);
                  end if;

               when Clear =>
                  Ada.Environment_Variables.Clear (N);
            end case;
            Maps.Next (Position);
         end Set_Var;
      end loop;

   exception
      when E : Constraint_Error =>
         Logs.Write (Content => "Unable to set environment",
                     Kind    => Logs.Handler.Error);
         Logs.Write (Content => "Set_Environment failed with " &
                     Exceptions.Exception_Information (E),
                     Kind    => Logs.Handler.Error);

   end Set_Environment;

end Savadur.Environment_Variables;
