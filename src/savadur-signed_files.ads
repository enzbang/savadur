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

   type Handler is private;

   subtype Signature is SHA.Strings.Hex_SHA_String;

   No_SHA1 : constant Signature;

   procedure Create (File : in out Handler; Name, Filename : in String);
   --  Creates a new signed file object referencing file Name

   function Exists (File : in Handler) return Boolean;
   --  Returns True if File exists

   function Name (File : in Handler) return String;
   --  Returns File's simple name

   function Full_Name (File : in Handler) return String;
   --  Returns File's full pathname

   function SHA1 (File : in Handler) return Signature;
   --  Returns File's SHA1 signature, compute it if not already done

   type External_Handler is new String;

   function To_Handler (File : in External_Handler) return Handler;
   --  Converts to an internal handler representation

   function To_External_Handler
     (File : in Handler) return External_Handler;
   --  Converts to an external handler representation for the Web Services

private

   No_SHA1 : constant Signature := Signature'(others => '0');

   type Handler is record
      Name      : Unbounded_String;
      Full_Name : Unbounded_String;
      Exists    : Boolean := False;
      SHA1      : Signature := No_SHA1;
   end record;

end Savadur.Signed_Files;
