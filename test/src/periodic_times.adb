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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

with Savadur.Times;

package body Periodic_Times is

   use Ada;

   procedure Check_Periodic_Times
     (T : in out AUnit.Test_Cases.Test_Case'Class);

   function HM (T : in Calendar.Time) return String;
   --  Returns the Hour and Minute part with format H:M

   function Equal
     (D1, D2 : in Duration; Epsilon : Duration := 1.0) return Boolean;
   --  Returns True if D1 and D2 are distant of less than Epsilon seconds

   --------------------------
   -- Check_Periodic_Times --
   --------------------------

   procedure Check_Periodic_Times
     (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use type Calendar.Time;
      pragma Unreferenced (T);
      Now : constant Calendar.Time := Calendar.Clock;
      P   : Savadur.Times.Periodic;
   begin
      --  Starting event before current time

      P := Savadur.Times.Create (HM (Now) & "/+90");

      delay 1.0;

      --  be sure the current time is passed, then next time should be in 89
      --  minutes and 59 seconds approximatively.

      Assertions.Assert
        (Equal (Savadur.Times.Next_Run_In (P), 89.0 * 60.0 + 59.0),
         "Wrong next_in_run '"
           & Duration'Image (Savadur.Times.Next_Run_In (P)) & ''');

      --  Starting event after time

      P := Savadur.Times.Create (HM (Now - (10.0 * 60.0)) & "/+90");

      --  So next event should be in 1h20m (80m)

      Assertions.Assert
        (Equal (Savadur.Times.Next_Run_In (P), 80.0 * 60.0),
         "Wrong next_in_run '"
           & Duration'Image (Savadur.Times.Next_Run_In (P)) & ''');
   end Check_Periodic_Times;

   -----------
   -- Equal --
   -----------

   function Equal
     (D1, D2 : in Duration; Epsilon : Duration := 1.0) return Boolean is
   begin
      return abs (D1 - D2) < Epsilon;
   end Equal;

   --------
   -- HM --
   --------

   function HM (T : in Calendar.Time) return String is
      I : constant String :=
            Calendar.Formatting.Image
              (T,
               Time_Zone => Calendar.Time_Zones.UTC_Time_Offset);
   begin
      return I (12 .. 16);
   end HM;

   ----------
   -- Name --
   ----------

   function Name (T : in Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Check the periodic times API");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Check_Periodic_Times'Access, "check periodic times");
   end Register_Tests;

end Periodic_Times;
