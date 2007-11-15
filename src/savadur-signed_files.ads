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

with SHA.Strings;

package Savadur.Signed_Files is

   use Ada;
   use Ada.Strings.Unbounded;

   type Handler is limited private;

   procedure Create (File : in out Handler; Name : in String);
   --  Create a new signed file object referencing file Name

   function Exists (File : in Handler) return Boolean;
   --  Returns True if File exists

   function Name (File : in Handler) return String;
   --  Returns File's simple name

   function Full_Name (File : in Handler) return String;
   --  Returns File's full pathname

   function SHA1 (File : in Handler) return SHA.Strings.Hex_SHA_String;
   --  Returns File's SHA1 signature, compute it if not already done

private

   No_SHA1 : constant SHA.Strings.Hex_SHA_String := (others => '0');

   type Handler is limited record
      Self      : access Handler := Handler'Unchecked_Access;
      Full_Name : Unbounded_String;
      Exists    : Boolean := False;
      SHA1      : SHA.Strings.Hex_SHA_String := No_SHA1;
   end record;

end Savadur.Signed_Files;
