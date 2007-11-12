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

with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Directories;

with Savadur.Utils;

package body Savadur.Config is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Directory : Unbounded_String;

   -----------------------
   -- Savadur_Directory --
   -----------------------

   function Savadur_Directory return String is
   begin
      if Directory /= Null_Unbounded_String then
         return -Directory;
      elsif Environment_Variables.Exists ("SAVADUR_DIR") then
         return Environment_Variables.Value ("SAVADUR_DIR");
      elsif Environment_Variables.Exists ("HOME") then
         return Directories.Compose
           (Containing_Directory => Environment_Variables.Value ("HOME"),
            Name                 => ".savadur");
      end if;

      --  All tries fail raise exception

      raise Config_Error with "No savadur directory found ! Use --savadur-dir "
        & "option or set SAVADUR_DIR environment variable";

   end Savadur_Directory;

   ---------------------------
   -- Set_Savadur_Directory --
   ---------------------------

   procedure Set_Savadur_Directory (Dir : in String) is
   begin
      Directory := +Dir;
   end Set_Savadur_Directory;

   --------------------
   -- Work_Directory --
   --------------------

   function Work_Directory return String is
      Work_Dir : constant String := Directories.Compose
        (Containing_Directory => Savadur_Directory,
         Name                 => "work");
   begin
      if not Directories.Exists (Name => Work_Dir) then
         Directories.Create_Path (New_Directory => Work_Dir);
      end if;

      return Work_Dir;
   end Work_Directory;

end Savadur.Config;
