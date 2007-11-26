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

package Savadur.Config is

   Config_Error : exception;

   function Savadur_Directory return String;
   --  Returns Savadur directory or raise Config_Error
   --  Savadur directory containing :
   --      client.xml
   --      projects/
   --      scm/
   --      env/
   --      servers/
   --      work/
   --           $(project_name)/
   --                           log

   procedure Set_Savadur_Directory (Dir : in String);
   --  Sets savadur directory (overwrite environment variables)

   function Work_Directory return String;
   --  Returns Work directory, creates it if it does not exist

   function SCM_Directory return String;
   --  Returns SCM directory,  creates it if it does not exist

   function Project_File_Directory return String;
   --  Returns project file directory $(savadur_dir)/projects
   --  Creates it if it does not exist

   function Project_Env_Directory return String;
   --  Returns project file directory $(savadur_dir)/env

   function Server_Directory return String;
   --  Returns server directory

   Client_Server : Boolean := False;
   --  Set to true if the client is configured to work with a server

end Savadur.Config;
