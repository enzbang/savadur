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

with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Directories;

with Morzhol.OS;

with Savadur.Utils;

package body Savadur.Config is

   use Ada;
   use Ada.Strings.Unbounded;

   use Savadur.Utils;

   Cached_Directory               : access String := null;
   Cached_Project_Env_Directory   : access String := null;
   Cached_Project_File_Directory  : access String := null;
   Cached_RSS_Directory           : access String := null;
   Cached_SCM_Directory           : access String := null;
   Cached_Server_Directory        : access String := null;
   Cached_Web_CSS_Directory       : access String := null;
   Cached_Web_Directory           : access String := null;
   Cached_Web_Templates_Directory : access String := null;
   Cached_Work_Dir                : access String := null;

   ---------------------------
   -- Project_Env_Directory --
   ---------------------------

   function Project_Env_Directory return String is
   begin
      if Cached_Project_Env_Directory = null then
         Cached_Project_Env_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Savadur_Directory,
               Name                 => "env"));
      end if;
      return Cached_Project_Env_Directory.all;
   end Project_Env_Directory;

   ----------------------------
   -- Project_File_Directory --
   ----------------------------

   function Project_File_Directory return String is
   begin
      if Cached_Project_File_Directory = null then
         Cached_Project_File_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Savadur_Directory,
               Name                 => "projects"));

         if not Directories.Exists (Cached_Project_File_Directory.all) then
            Directories.Create_Path (Cached_Project_File_Directory.all);
         end if;
      end if;

      return Cached_Project_File_Directory.all;
   end Project_File_Directory;

   -------------------
   -- RSS_Directory --
   -------------------

   function RSS_Directory return String is
   begin
      if Cached_RSS_Directory = null then
         Cached_RSS_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Web_Directory,
               Name                 => "rss"));
      end if;

      return Cached_RSS_Directory.all;
   end RSS_Directory;

   -----------------------
   -- Savadur_Directory --
   -----------------------

   function Savadur_Directory return String is
   begin
      if Cached_Directory = null then
         if Environment_Variables.Exists ("SAVADUR_DIR") then
            Cached_Directory := new String'
              (Environment_Variables.Value ("SAVADUR_DIR"));

         elsif Environment_Variables.Exists ("HOME") then
            Cached_Directory := new String'
              (Directories.Compose
                 (Containing_Directory => Environment_Variables.Value ("HOME"),
                  Name                 => ".savadur"));
         else
            --  All tries fail raise exception

            raise Config_Error
              with "No savadur directory found ! Use --savadur-dir "
                & "option or set SAVADUR_DIR environment variable";
         end if;
      end if;

      return Cached_Directory.all;
   end Savadur_Directory;

   -------------------
   -- SCM_Directory --
   -------------------

   function SCM_Directory return String is
   begin
      if Cached_SCM_Directory = null then
         Cached_SCM_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Config.Savadur_Directory,
               Name                 => "scm"));

         if not Directories.Exists (Cached_SCM_Directory.all) then
            Directories.Create_Path (Cached_SCM_Directory.all);
         end if;
      end if;

      return Cached_SCM_Directory.all;
   end SCM_Directory;

   ----------------------
   -- Server_Directory --
   ----------------------

   function Server_Directory return String is
   begin
      if Cached_Server_Directory = null then
         Cached_Server_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Config.Savadur_Directory,
               Name                 => "servers"));

         if not Directories.Exists (Cached_Server_Directory.all) then
            Directories.Create_Path (Cached_Server_Directory.all);
         end if;
      end if;

      return Cached_Server_Directory.all;
   end Server_Directory;

   ---------------------------
   -- Set_Savadur_Directory --
   ---------------------------

   procedure Set_Savadur_Directory (Dir : in String) is
   begin
      Cached_Directory := new String'
        (Morzhol.OS.Compose (Directories.Current_Directory, Dir));
   end Set_Savadur_Directory;

   -----------------------
   -- Web_CSS_Directory --
   -----------------------

   function Web_CSS_Directory return String is
   begin
      if Cached_Web_CSS_Directory = null then
         Cached_Web_CSS_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Web_Directory,
               Name                 => "htdocs"));
      end if;

      return Cached_Web_CSS_Directory.all;
   end Web_CSS_Directory;

   -------------------
   -- Web_Directory --
   -------------------

   function Web_Directory return String is
   begin
      if Cached_Web_Directory = null then
         Cached_Web_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Savadur_Directory,
               Name                 => "htdocs"));
      end if;

      return Cached_Web_Directory.all;
   end Web_Directory;

   -----------------------------
   -- Web_Templates_Directory --
   -----------------------------

   function Web_Templates_Directory return String is
   begin
      if Cached_Web_Templates_Directory = null then
         Cached_Web_Templates_Directory := new String'
           (Directories.Compose
              (Containing_Directory => Web_Directory,
               Name                 => "templates"));
      end if;

      return Cached_Web_Templates_Directory.all;
   end Web_Templates_Directory;


   --------------------
   -- Work_Directory --
   --------------------

   function Work_Directory return String is
   begin
      if Cached_Work_Dir = null then
         Cached_Work_Dir := new String'
           (Directories.Compose
              (Containing_Directory => Savadur_Directory,
               Name                 => "work"));

         if not Directories.Exists (Name => Cached_Work_Dir.all) then
            Directories.Create_Path (New_Directory => Cached_Work_Dir.all);
         end if;
      end if;

      return Cached_Work_Dir.all;
   end Work_Directory;

end Savadur.Config;
