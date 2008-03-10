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

with Ada.Strings.Hash;

package body Savadur.Actions is

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Action) return Containers.Hash_Type is
   begin
      return Hash (Key.Id);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Id) return Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Key));
   end Hash;
   -----------
   -- Image --
   -----------

   function Image (Action : in Actions.Action) return String is
      use type Config.Cmd.Output_Pattern;
      R : Unbounded_String;
   begin
      R := R & To_String (Action.Id) & " => "
        & Config.Cmd.External_Command_Utils.To_String (Action.Cmd.Cmd);

      if Action.Cmd.Output /= Config.Cmd.Output_Pattern_Utils.Nil then
         R := R & " (" &
           Config.Cmd.Output_Pattern_Utils.To_String (Action.Cmd.Output) & ')';
      end if;

      R := R & " " & " result type : "
        & Result_Type'Image (Action.Result) & ASCII.LF;

      return To_String (R);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Action : in Ref_Action) return String is
      Result : Unbounded_String :=
                 +Kind'Image (Action.Action_Type)
                 & " " & To_String (Action.Id);
   begin
      if Action.Value /= Null_Unbounded_String then
         Append (Result, " wanted value=" & (-Action.Value));
      end if;

      if Action.Require_Change then
         Append (Result, " require_change");
      end if;

      if Action.On_Error /= Error then
         Append (Result,
                 " on error = " & On_Error_Hook'Image (Action.On_Error));
      end if;
      return -Result;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Set : in Sets.Set) return String is

      Result : Unbounded_String := +"[" & ASCII.LF;

      procedure Image (Position : in Sets.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Sets.Cursor) is
      begin
         Append (Result, Image (Sets.Element (Position)));
      end Image;

   begin
      Sets.Iterate (Container => Set, Process => Image'Access);
      Append (Result, "]");
      return -Result;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Vector : in Vectors.Vector) return String is

      Result : Unbounded_String := +"[" & ASCII.LF;

      procedure Image (Position : in Vectors.Cursor);
      --  Adds position image

      -----------
      -- Image --
      -----------

      procedure Image (Position : in Vectors.Cursor) is
         Element : constant Ref_Action := Vectors.Element (Position);
      begin
         Append (Result, Image (Element) & ASCII.LF);
      end Image;

   begin
      Vectors.Iterate (Container => Vector, Process => Image'Access);
      Append (Result, +"]");
      return -Result;
   end Image;

   ---------
   -- Key --
   ---------

   function Key (Element : in Action) return Id is
   begin
      return Element.Id;
   end Key;

end Savadur.Actions;
