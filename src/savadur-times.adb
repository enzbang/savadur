------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2009                          --
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
with Ada.Strings.Fixed;

with Savadur.Utils;

package body Savadur.Times is

   use Ada.Strings;
   use Savadur.Utils;

   ------------
   -- Create --
   ------------

   function Create (From : in String) return Periodic is
      Now : constant Calendar.Time := Calendar.Clock;
      S1  : constant Natural := Fixed.Index (From, ":");
      S2  : constant Natural := Fixed.Index (From, "/");
   begin
      if S1 = 0 or else S2 = 0 then
         raise Constraint_Error
           with "non conforming periodic time '" & From & ''';
      else

         --  Add a local exception handler to avoid catching the previous
         --  Contraint_Error.

         Safe_Creation : begin
            Create_Perodic : declare
               H : constant Calendar.Formatting.Hour_Number :=
                     Calendar.Formatting.Hour_Number'Value
                       (From (From'First .. S1 - 1));
               M : constant Calendar.Formatting.Minute_Number :=
                     Calendar.Formatting.Minute_Number'Value
                       (From (S1 + 1 .. S2 - 1));
            begin
               return Periodic'
                 (Event => Calendar.Time_Of
                    (Year    => Calendar.Year (Now),
                     Month   => Calendar.Month (Now),
                     Day     => Calendar.Day (Now),
                     Seconds => H * 3600.0 + M * 60.0 +
                       Duration (Calendar.Formatting.Second (Now))),
                  Every => Duration'Value (From (S2 + 2 .. From'Last)) * 60.0,
                  From  => +From);
            end Create_Perodic;
         exception
            when others =>
               raise Constraint_Error
                 with "non conforming periodic time '" & From & '''
                   & " (format is HH:MM/+X - run at HH:MM every X minutes)";
         end Safe_Creation;
      end if;
   end Create;

   -----------
   -- Image --
   -----------

   function Image (Time : in Periodic) return String is
   begin
      return -Time.From;
   end Image;

   -----------------
   -- Next_Run_At --
   -----------------

   function Next_Run_At (Time : in Periodic) return Calendar.Time is
      use type Calendar.Time;
   begin
      return Calendar.Clock + Next_Run_In (Time);
   end Next_Run_At;

   -----------------
   -- Next_Run_In --
   -----------------

   function Next_Run_In (Time : in Periodic) return Duration is
   begin
      if Time = No_Time then
         return 0.0;

      else
         Compute_Delay : declare
            Now     : constant Calendar.Time := Calendar.Clock;
            S_Now   : constant Duration := Calendar.Seconds (Now);
            S_Event : Duration := Calendar.Seconds (Time.Event);
            Elapsed : constant Duration := S_Now - S_Event;
         begin
            if Elapsed > 0.0 then
               --  Now is passed the current event
               while S_Event < S_Now loop
                  S_Event := S_Event + Time.Every;
               end loop;

               return S_Event - S_Now;

            else
               --  Event is in the futur
               while S_Event > S_Now loop
                  S_Event := S_Event - Time.Every;
               end loop;
               S_Event := S_Event + Time.Every;

               return S_Event - S_Now;
            end if;
         end Compute_Delay;
      end if;
   end Next_Run_In;

end Savadur.Times;
