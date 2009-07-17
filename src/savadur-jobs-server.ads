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
with Savadur.Jobs.Queue;
with Savadur.Projects;
with Savadur.Scenarios;

package Savadur.Jobs.Server is

   function Run
     (Project        : access Projects.Project_Config;
      Patch_Filename : in     String;
      Server         : in     String;
      Env_Var        : in     Environment_Variables.Containers.Maps.Map;
      Scenario       : in     Scenarios.Id;
      Id             : in     Natural := 0) return Scenarios.Run_Status;
   --  This is the server side run routine, it does not call directly the
   --  Build.Run routine as nothing is actually run on the server but send run
   --  requests to the clients.

   package Queue is new Savadur.Jobs.Queue (Run => Run);

end Savadur.Jobs.Server;
