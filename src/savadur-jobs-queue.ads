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

with Savadur.Environment_Variables.Containers;
with Savadur.Projects;
with Savadur.Scenarios;
with Savadur.Signed_Files;
with Savadur.Times;

generic
   with function Run
     (Project  : access Projects.Project_Config;
      Patch    : in     String;
      Server   : in     String;
      Env_Var  : in     Environment_Variables.Containers.Maps.Map;
      Scenario : in     Scenarios.Id;
      Id       : in     Natural) return Scenarios.Run_Status;
package Savadur.Jobs.Queue is

   procedure Add
     (Project  : in Signed_Files.Handler;
      Patch    : in String;
      Server   : in String;
      Scenario : in String;
      Time     : in Times.Periodic := Times.No_Time;
      Latency  : in Duration := 1.0;
      Id       : in Natural := 0);
   --  Schedules a new job from the given server.
   --  10 minutes default latency (time before starting a non periodic job).
   --  Each request to build the same scenario will push the build in time to
   --  at least wait for Latency seconds.

   procedure Add_Periodic_Scenario;
   --  Adds all known periodic scenarios found in loaded projects into the task
   --  queue. It is fine to call this routine multiple times as only new
   --  periodic tasks will get queued.

   procedure Stop;
   --  Sends a stop signal to the job task. All currently registered jobs will
   --  be terminated first.

   function Next_Job_In (Project : in Signed_Files.Handler) return Duration;
   --  Returns number of seconds for next job

end Savadur.Jobs.Queue;
