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

with Savadur.Environment_Variables;
with Savadur.Projects;
with Savadur.Scenarios;
with Savadur.Signed_Files;
with Savadur.Times;

generic
   with function Run
     (Project  : access Projects.Project_Config;
      Env_Var  : in     Environment_Variables.Maps.Map;
      Scenario : in     Scenarios.Id) return Boolean;
package Savadur.Jobs.Queue is

   procedure Add
     (Project  : in Signed_Files.Handler;
      Scenario : in String;
      Time     : in Times.Periodic := Times.No_Time);
   --  Schedules a new job

   procedure Add_Periodic_Scenario;
   --  Adds all known periodic scenarios found in loaded projects into the task
   --  queue. It is fine to call this routine multiple times as only new
   --  periodic tasks will get queued.

   procedure Stop;
   --  Sends a stop signal to the job task. All currently registered jobs will
   --  be terminated first.

end Savadur.Jobs.Queue;