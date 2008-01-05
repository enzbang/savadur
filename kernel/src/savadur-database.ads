------------------------------------------------------------------------------
--                                Savadur                                   --
--                                                                          --
--                        Copyright (C) 2007-2008                           --
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

with AWS.Templates;

package Savadur.Database is

   use AWS;

   No_Database : exception;

   procedure Log
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Action       : in String;
      Output       : in String;
      Result       : in Boolean;
      Job_Id       : in Natural);
   --  Adds log to database

   function Get_Logs
     (Project_Name : in String) return Templates.Translate_Set;
   --  Get logs

   function Get_Log_Content (Id : in Positive) return Templates.Translate_Set;
   --  Get log content

   procedure Final_Status
     (Key          : in String;
      Project_Name : in String;
      Scenario     : in String;
      Result       : in Boolean;
      Job_Id       : in Natural);
   --  Adds final status to database

   function Get_Final_Status
     (Project_Name : in String) return Templates.Translate_Set;
   --  Get final status

   procedure Login (Key : in String);
   --  Client has login

   procedure Logout (Key : in String);
   --  Client has logout

   function Job_Id return Positive;
   --  Get new job id

end Savadur.Database;
