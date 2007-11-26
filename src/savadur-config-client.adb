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

with Savadur.Utils;

package body Savadur.Config.Client is

   use Ada.Strings.Unbounded;

   type Config is record
      Key : Unbounded_String;
   end record;

   Configuration : Config;

   Empty : constant Config := Config'(Key => Null_Unbounded_String);

   -------------
   -- Get_Key --
   -------------

   function Get_Key return String is
      use Savadur.Utils;

      procedure Parse;
      --  Parses client configuration file

      -----------
      -- Parse --
      -----------

      procedure Parse is
      begin
         --  ??? TBD

         Configuration.Key := +"me";
      end Parse;

   begin
      if Configuration = Empty then
         Parse;
      end if;

      return -Configuration.Key;
   end Get_Key;

end Savadur.Config.Client;
