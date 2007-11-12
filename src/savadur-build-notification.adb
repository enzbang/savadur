------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
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

package body Savadur.Build.Notification is

   use Ada;

   ------------
   -- Notify --
   ------------

   procedure Notify
     (Project : access Config.Project.Project_Config;
      Success : in     Boolean)
   is
      Log_Directory : constant String :=
                        Config.Project.Project_Log_Directory
                          (Project.Project_Id);

   begin
      if Success then
         for K in 1 .. Project.Notifications.On_Success.Length loop
            declare
               Ref         : constant Actions.Ref_Action :=
                               Project.Notifications.On_Success.Element
                                 (Integer (K));
               Exec_Action : constant Actions.Action :=
                               Build.Get_Action
                                 (Project    => Project.all,
                                  Ref_Action => Ref);

               Log_File    : constant String := Directories.Compose
                 (Containing_Directory => Log_Directory,
                  Name                 => "on_success_"
                  & Actions.Id_Utils.To_String (Ref.Id));
               Return_Code : Integer;
               Result      : Boolean;
            begin
               Execute (Exec_Action => Exec_Action,
                        Directory   => Config.Savadur_Directory,
                        Log_Filename  => Log_File,
                        Return_Code   => Return_Code,
                        Result        => Result);
            end;
         end loop;
      else
         for K in 1 .. Project.Notifications.On_Failure.Length loop
            declare
               Ref         : constant Actions.Ref_Action :=
                               Project.Notifications.On_Failure.Element
                                 (Integer (K));
               Exec_Action : constant Actions.Action := Get_Action
                 (Project    => Project.all,
                  Ref_Action => Ref);

               Log_File    : constant String := Directories.Compose
                 (Containing_Directory => Log_Directory,
                  Name                 => "on_failure_"
                  & Actions.Id_Utils.To_String (Ref.Id));
               Return_Code : Integer;
               Result      : Boolean;
            begin
               Execute (Exec_Action => Exec_Action,
                        Directory   => Config.Savadur_Directory,
                        Log_Filename  => Log_File,
                        Return_Code   => Return_Code,
                        Result        => Result);
            end;
         end loop;
      end if;

   end Notify;

end Savadur.Build.Notification;
