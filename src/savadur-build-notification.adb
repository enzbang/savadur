------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                         Copyright (C) 2007-2008                          --
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

with Ada.Directories;

with Savadur.Actions;
with Savadur.Config;

package body Savadur.Build.Notification is

   use Ada;

   ------------
   -- Notify --
   ------------

   procedure Notify
     (Project : access Projects.Project_Config;
      Success : in     Scenarios.Run_Status;
      Job_Id  : in     Natural)
   is
      Log_Directory : constant String :=
                        Projects.Project_Log_Directory (Project);
   begin
      case Success is
         when Scenarios.Success =>
            for K in 1 .. Project.Notifications.On_Success.Length loop
               Notify_Success : declare
                  Ref         : constant Actions.Ref_Action :=
                                  Project.Notifications.On_Success.Element
                                    (Integer (K));
                  Exec_Action : constant Actions.Action :=
                                  Build.Get_Action
                                    (Project    => Project.all,
                                     Ref_Action => Ref);
                  Log_File    : constant String :=
                                  Log_Filename
                                    (Project   => Project,
                                     Action_Id => Ref.Id,
                                     Job_Id    => Job_Id,
                                     Prefix    => "on_success_");
                  Return_Code : Integer;
                  Result      : Boolean;
               begin
                  Execute (Exec_Action   => Exec_Action,
                           Directory     => Config.Savadur_Directory,
                           Log_Filename  => Log_File,
                           Return_Code   => Return_Code,
                           Result        => Result);
               end Notify_Success;
            end loop;

         when Scenarios.Failure =>
            for K in 1 .. Project.Notifications.On_Failure.Length loop
               Notify_Failure : declare
                  Ref         : constant Actions.Ref_Action :=
                                  Project.Notifications.On_Failure.Element
                                    (Integer (K));
                  Exec_Action : constant Actions.Action := Get_Action
                    (Project    => Project.all,
                     Ref_Action => Ref);
                  Log_File    : constant String :=
                                  Log_Filename
                                    (Project   => Project,
                                     Action_Id => Ref.Id,
                                     Job_Id    => Job_Id,
                                     Prefix    => "on_failure_");
                  Return_Code : Integer;
                  Result      : Boolean;
               begin
                  Execute (Exec_Action   => Exec_Action,
                           Directory     => Config.Savadur_Directory,
                           Log_Filename  => Log_File,
                           Return_Code   => Return_Code,
                           Result        => Result);
               end Notify_Failure;
            end loop;

         when Scenarios.Skipped =>
            --  ??? add a notification for skipped status ?
            null;
      end case;
   end Notify;

end Savadur.Build.Notification;
