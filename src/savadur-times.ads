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

with Ada.Calendar;
with Ada.Strings.Unbounded;

package Savadur.Times is

   use Ada;
   use Ada.Strings.Unbounded;

   type Periodic is private;

   function Create (From : in String) return Periodic;
   --  Convert a periodic time external representation to a periodic object.
   --  Current supported format is:
   --     H:M/+H   - event sceduled at H:M, run every H hours
   --  Raises Constraint_Error if format is not recognized.

   function Image (Time : in Periodic) return String;
   --  Returns the string representation of the given periodic time. This
   --  routine is the exact opposite of the create routine above.

   function Next_Run_In (Time : in Periodic) return Duration;
   --  Returns the duration until next events

   function Next_Run_At (Time : in Periodic) return Calendar.Time;
   --  Returns the time at which next event will be sceduled

private

   type Periodic is record
      From  : Unbounded_String;
      Event : Calendar.Time;
      Every : Duration;
   end record;

end Savadur.Times;
