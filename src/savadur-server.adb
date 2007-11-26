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

--  Usage :
--
--  savadur-server [OPTIONS]
--
--  OPTIONS :
--       -savadurdir dirname : Set savadur directory
--                             ($SAVADUR_DIR or $HOME / .savadur by default)
--       -v|-version
--       -V|-verbose
--       -VV|-very_verbose
--       -L filename         : use filename for log file

with Ada.Command_Line;
with GNAT.Command_Line;

with Savadur.Config.Project;
with Savadur.Config.SCM;
with Savadur.Logs;
with Savadur.Version;
with Savadur.Web.Server;

procedure Savadur.Server is

   use Ada;

   Syntax_Error : exception;

   procedure Usage (Error_Message : in String := "");
   --  Displays usage

   -----------
   -- Usage --
   -----------

   procedure Usage (Error_Message : in String := "") is
   begin
      --  Display error message if not null

      if Error_Message /= "" then
         Logs.Write (Content => Error_Message, Kind => Logs.Error);

         --  Set exit status to Failure
         Command_Line.Set_Exit_Status (Command_Line.Failure);
      end if;

      Logs.Write ("Savadur server " & Version.Simple);
      Logs.Write ("usage : savadur-server [OPTIONS]");
      Logs.Write ("OPTIONS :");
      Logs.Write ("    -savadurdir dirname : set savadur directory");
      Logs.Write ("          ($SAVADUR_DIR or $HOME/.savadur by default)");
      Logs.Write ("    -v|-version");
      Logs.Write ("    -V|-verbose");
      Logs.Write ("    -VV|-very_verbose");
      Logs.Write ("    -L filename         : use filename for log file");
   end Usage;

begin

   Iterate_On_Opt : loop
      case GNAT.Command_Line.Getopt
           ("V verbose VV very_verbose L: version v savadurdir:")
         is
         when ASCII.NUL =>
            exit Iterate_On_Opt;

         when 's' =>
            Complete_S : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "savadurdir" then
                  Config.Set_Savadur_Directory
                    (GNAT.Command_Line.Parameter);
               end if;
            end Complete_S;

         when 'v' | 'V' =>
            Complete_V : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "verbose" or else Full = "V"  then
                  Logs.Set (Kind => Logs.Verbose, Activated => True);

               elsif Full = "very_verbose" or else Full = "VV" then
                  Logs.Set (Kind      => Logs.Verbose,
                            Activated => True);
                  Logs.Set (Kind      => Logs.Very_Verbose,
                            Activated => True);

               elsif Full = "version" or else Full = "v" then
                  Logs.Write (Content => "Savadur "
                              & Savadur.Version.Complete);
                  return;

               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_V;

         when 'L' =>
            Complete_L : declare
               Full : constant String := GNAT.Command_Line.Full_Switch;
            begin
               if Full = "L" then
                  Logs.Set_File (GNAT.Command_Line.Parameter);
               else
                  raise Syntax_Error with "Unknown option " & Full;
               end if;
            end Complete_L;

         when others =>
            Usage (Error_Message => "unknown syntax");
            return;
      end case;
   end loop Iterate_On_Opt;

   Config.SCM.Parse;
   Config.Project.Parse;

   Web.Server.Start;

   --  Endless loop

   loop
      delay Duration'Last;
   end loop;

exception
   when GNAT.Command_Line.Invalid_Section
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter =>
      Usage ("unknown syntax");
   when E : Savadur.Config.Config_Error
      | Savadur.Config.Project.Config_Error =>
      Usage (Error_Message => Exceptions.Exception_Message (E));
end Savadur.Server;
