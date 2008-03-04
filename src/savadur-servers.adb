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

with Ada.Strings.Hash;

with Savadur.Utils;

---------------------
-- Savadur.Servers --
---------------------

package body Savadur.Servers is

   use Savadur.Utils;

   All_Servers : Sets.Set;

   ---------
   -- Get --
   ---------

   function Get (Name : in String) return Server is
      Position : constant Sets.Cursor :=
                   Sets.Find
                     (Container => All_Servers,
                      Item      => Server'(Name     => +Name,
                                           URL      => <>,
                                           Log_Path => <>,
                                           Send_Log => <>,
                                           Status   => <>));
   begin
      return Sets.Element (Position);
   end Get;

   ----------------
   -- Go_Offline --
   ----------------

   procedure Go_Offline (Server_Name : in Unbounded_String) is
      Position : constant Sets.Cursor :=
                   Sets.Find
                     (Container => All_Servers,
                      Item      => Server'(Name  => Server_Name,
                                           URL    => <>,
                                           Log_Path => <>,
                                           Send_Log => <>,
                                           Status => <>));
      Element  : Server := Sets.Element (Position);
   begin
      Element.Status := Offline;
      Sets.Replace_Element
        (Container => All_Servers, Position => Position, New_Item => Element);
   end Go_Offline;

   ---------------
   -- Go_Online --
   ---------------

   procedure Go_Online (Server_Name : in Unbounded_String) is
      Position : constant Sets.Cursor :=
                   Sets.Find
                     (Container => All_Servers,
                      Item      => Server'(Name     => Server_Name,
                                           URL      => <>,
                                           Log_Path => <>,
                                           Send_Log => <>,
                                           Status   => <>));
      Element  : Server := Sets.Element (Position);
   begin
      Element.Status := Online;
      Sets.Replace_Element
        (Container => All_Servers, Position => Position, New_Item => Element);
   end Go_Online;

   ----------
   -- Hash --
   ----------

   function Hash (Server : in Servers.Server) return Containers.Hash_Type is
   begin
      return Strings.Hash (To_String (Server.Name));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image return String is
   begin
      return Image (All_Servers);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Servers_Set : in Sets.Set) return String is
      Position : Sets.Cursor := Sets.First (Servers_Set);
      Result   : Unbounded_String;
   begin
      while Sets.Has_Element (Position) loop
         One_Server : declare
            S : constant Server := Sets.Element (Position);
         begin
            Append
              (Result, "* " & To_String (S.Name) & ASCII.LF);
            Append (Result, "[" & ASCII.LF);
            Append (Result, "Name => " & To_String (S.Name) & ASCII.LF);
            Append (Result, "URL => " & To_String (S.URL) & ASCII.LF);

            if S.Log_Path /= Null_Unbounded_String then
               Append
                 (Result, "Log_Path => " & To_String (S.Log_Path) & ASCII.LF);
            end if;

            Append
              (Result, "Send_Log => " & Boolean'Image (S.Send_Log) & ASCII.LF);

            Append (Result, "]" & ASCII.LF);
         end One_Server;
         Sets.Next (Position);
      end loop;

      return -Result;
   end Image;

   ------------
   -- Insert --
   ------------

   procedure Insert (Name, URL, Log_Path : in String; Send_Log : in Boolean) is
      New_Item : constant Server :=
                   Server'(+Name, +URL, +Log_Path, Send_Log, Offline);
   begin
      Sets.Insert (Container => All_Servers, New_Item => New_Item);
   end Insert;

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (S1, S2 : in Server) return Boolean is
   begin
      return S1.Name = S2.Name;
   end Key_Equal;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      return Natural (Sets.Length (All_Servers));
   end Length;

   --------------
   -- Log_Path --
   --------------

   function Log_Path (Server : in Servers.Server) return String is
   begin
      return -Server.Log_Path;
   end Log_Path;

   ----------
   -- Name --
   ----------

   function Name (Position : in Cursor) return String is
      Element : constant Server := Sets.Element (Sets.Cursor (Position));
   begin
      return -Element.Name;
   end Name;

   ---------------------
   -- Offline_Iterate --
   ---------------------

   procedure Offline_Iterate
     (Process : not null access procedure (Position : in Cursor))
   is
      Position : Sets.Cursor := Sets.First (All_Servers);
   begin
      while Sets.Has_Element (Position) loop
         if Sets.Element (Position).Status = Offline then
            Process (Cursor (Position));
         end if;
         Sets.Next (Position);
      end loop;
   end Offline_Iterate;

   --------------------
   -- Online_Iterate --
   --------------------

   procedure Online_Iterate
     (Process : not null access procedure (Position : in Cursor))
   is
      Position : Sets.Cursor := Sets.First (All_Servers);
   begin
      while Sets.Has_Element (Position) loop
         if Sets.Element (Position).Status = Online then
            Process (Cursor (Position));
         end if;
         Sets.Next (Position);
      end loop;
   end Online_Iterate;

   --------------
   -- Send_Log --
   --------------

   function Send_Log (Server : in Servers.Server) return Boolean is
   begin
      return Server.Send_Log;
   end Send_Log;

   ---------
   -- URL --
   ---------

   function URL (Position : in Cursor) return String is
      Element : constant Server := Sets.Element (Sets.Cursor (Position));
   begin
      return -Element.URL;
   end URL;

   ---------
   -- URL --
   ---------

   function URL (Server : in Servers.Server) return String is
   begin
      if Server.Status = Online then
         return -Server.URL;
      else
         return "";
      end if;
   end URL;

end Savadur.Servers;
