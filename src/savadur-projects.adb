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

with Ada.Directories;

with Savadur.Config;

package body Savadur.Projects is

   use Ada;
   use Savadur.Utils;

   -----------------------
   -- Project_Directory --
   -----------------------

   function Project_Directory
     (Project : access Project_Config) return String is
   begin
      if Project.Directory.Cached_Project_Directory = null then
         Project.Directory.Cached_Project_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Config.Work_Directory,
               Name                 => -Unbounded_String
                 (Project.Project_Id)));
         if not Directories.Exists
           (Name => Project.Directory.Cached_Project_Directory.all)
         then
            Directories.Create_Path
              (New_Directory => Project.Directory.Cached_Project_Directory.all);
         end if;
      end if;

      return Project.Directory.Cached_Project_Directory.all;
   end Project_Directory;

   --------------------------
   -- Project_Env_Filename --
   --------------------------

   function Project_Env_Filename
     (Project : access Project_Config) return String is
   begin
      if Project.Directory.Cached_Project_Env_Filename = null then
         Project.Directory.Cached_Project_Env_Filename := new String'
           (Directories.Compose
              (Containing_Directory => Config.Project_Env_Directory,
               Name                 => Id_Utils.To_String
                 (Project.Project_Id),
               Extension            => "xml"));
      end if;

      return Project.Directory.Cached_Project_Env_Filename.all;
   end Project_Env_Filename;

   ----------------------
   -- Project_Filename --
   ----------------------

   function Project_Filename
     (Project : access Project_Config) return String is
   begin
      if Project.Directory.Cached_Project_Filename = null then
         Project.Directory.Cached_Project_Filename := new String'
           (Directories.Compose
              (Containing_Directory => Config.Project_File_Directory,
               Name                 => Id_Utils.To_String
                 (Project.Project_Id),
               Extension            => "xml"));
      end if;

      return Project.Directory.Cached_Project_Filename.all;
   end Project_Filename;

   ---------------------------
   -- Project_Log_Directory --
   ---------------------------

   function Project_Log_Directory
     (Project : access Projects.Project_Config) return String is
   begin
      if Project.Directory.Cached_Project_Log_Directory = null then
         Project.Directory.Cached_Project_Log_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Project_Directory (Project),
               Name                 => "log"));
         if not Directories.Exists
           (Name => Project.Directory.Cached_Project_Log_Directory.all) then
            Directories.Create_Path
              (New_Directory => Project.Directory.Cached_Project_Log_Directory.all);
         end if;
      end if;

      return Project.Directory.Cached_Project_Log_Directory.all;
   end Project_Log_Directory;

   -------------------------------
   -- Project_Sources_Directory --
   -------------------------------

   function Project_Sources_Directory
     (Project : access Project_Config) return String is
   begin

      if Project.Directory.Cached_Project_Sources_Directory = null then
         Set_Dir : declare
            Var : constant Savadur.Variables.Variable :=
                    Savadur.Variables.Keys.Element
                      (Container  => Project.Variables,
                       Key        => Savadur.Variables.Name_Utils.Value
                         (Img => "sources"));
         begin
            Project.Directory.Cached_Project_Sources_Directory := new String'
              (Directories.Compose
                 (Containing_Directory => Project_Directory (Project),
                  Name                 => To_String (Var.Value)));
         end Set_Dir;
      end if;

      return Project.Directory.Cached_Project_Sources_Directory.all;
   end Project_Sources_Directory;

   -----------------------------
   -- Project_State_Directory --
   -----------------------------

   function Project_State_Directory
     (Project : access Projects.Project_Config) return String is
   begin
      if Project.Directory.Cached_Project_State_Directory = null then
         Project.Directory.Cached_Project_State_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Project_Directory (Project),
               Name                 => "state"));
         if not Directories.Exists
           (Name => Project.Directory.Cached_Project_State_Directory.all) then
            Directories.Create_Path
              (New_Directory => Project.Directory.Cached_Project_State_Directory.all);
         end if;
      end if;

      return Project.Directory.Cached_Project_State_Directory.all;
   end Project_State_Directory;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Project  : access Project_Config; Filename : in String) is
   begin
      Project.Directory.Cached_Project_Filename := new String'(Filename);
   end Set_Filename;

end Savadur.Projects;
