------------------------------------------------------------------------
--
--  Copyright (c) 2018, Brendan T Malone All Rights Reserved.
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--
------------------------------------------------------------------------

------------------------------------------------------------------------
--
--  Ada support for leveled logs. Based on Google's glog
--
--  Usage Stuff
--
------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
--  with GNAT.Sockets;

package body Alog is

   package AC  renames Ada.Calendar;
   package ACF renames Ada.Calendar.Formatting;
   package ACL renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package AEV renames Ada.Environment_Variables;
   package ASU renames Ada.Strings.Unbounded;

   ---------------------------------------------------------------------
   --  Private method declarations
   ---------------------------------------------------------------------

   procedure Create_Files;

   function Format_Output (Lvl : Level; Msg : String) return String;
   procedure Output (Lvl : Level; Msg : String);
   procedure Output_Stdout (Lvl : Level; Msg : String);
   procedure Output_File (Lvl : Level; Msg : String);

   ---------------------------------------------------------------------
   --  Import C menthods
   ---------------------------------------------------------------------

   --  Import getpid so that we can have the log files name be distinct
   --  per process.
   function Get_PID return Integer;
   pragma Import
     (Convention    => C,
      Entity        => Get_PID,
      External_Name => "getpid");

   ---------------------------------------------------------------------
   --  Package variables
   ---------------------------------------------------------------------

   --  By default only log to the console if the log message is an Error
   --  or greater.
   Stdout_Threshold : Level := ERROR;

   --  By default write both to the log files and the console.
   Log_Location : LogTo := BOTH;

   --  By default write all log files to the tmp directory.
   File_Location : ASU.Unbounded_String;

   --  Array of a file handler for each log level.
   type Log_Files is array (Level) of TIO.File_Type;
   Files : Log_Files;

   --  Mutex so the log can be multithreaded.
   protected type Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end Mutex;

   protected body Mutex is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;
      procedure Release is
      begin
         Owned := False;
      end Release;
   end Mutex;

   --  Instance of the lock.
   Lock : Mutex;

   ---------------------------------------------------------------------
   --  Private method
   ---------------------------------------------------------------------

   --  Create the log file in the form of:
   --  <program name>.<host>.<user>.log.<LEVEL>.<time>.<pid>
   --  in the defined file location.
   procedure Create_Files is
      Cmd  : constant String := Program_Name (ACL.Command_Name);
      Host : constant String := "hostname"; --  GNAT.Sockets.Host_Name;
      User : constant String := AEV.Value ("USER");
      Time : constant String := Program_Time (ACF.Image (AC.Clock));
      Pid  : constant String := Integer'Image (Get_PID);
      Prefix : constant String := Cmd & "." &
                           Host & "." & User & ".log.";
      Suffix : constant String := "." & Time & "." &
                          (Pid (Pid'First + 1 .. Pid'Last));
   begin
      if not Files_Location_Set then
         File_Location := ASU.To_Unbounded_String ("/tmp/");
      end if;
      for Lvl in Files'Range loop
         begin
            TIO.Create (File => Files (Lvl),
                        Mode => TIO.Out_File,
                        Name => ASU.To_String (File_Location) &
                           Prefix & Level'Image (Lvl) & Suffix);
            exception
               when others =>
                  raise Program_Error with "UNABLE TO CREATE LOGS";
         end;
      end loop;
      Files_Created := True;
   end Create_Files;

   --  Take the time the program was run and create a file friendly
   --  representation of the string.
   --  e.g 2018-12-01 08:08:20 -> 20181201.080820
   function Program_Time (Time : String) return String is
      Str : String (1 .. 15);
      Pos : Natural := Str'First;
   begin
      for i in Time'Range loop
         case Time (i) is
            when '-' =>
               null;
            when ':' =>
               null;
            when ' ' =>
               Str (Pos) := '.';
               Pos := Pos + 1;
            when others =>
               Str (Pos) := Time (i);
               Pos := Pos + 1;
         end case;
      end loop;
      return Str;
   end Program_Time;

   --  Take the time the command name and remove all the dots and slashes
   --  so the program name can be used in the log file name.
   function Program_Name (Cmd : String) return String is
      Pos : Integer := 0;
   begin
      for i in Cmd'Range loop
         --  Go backwards through the string till a / is found
         if Cmd (Cmd'Last - i) = '/' then
            Pos := i;
            exit;
         end if;
      end loop;
      return (Cmd (Cmd'Last - Pos + 1 .. Cmd'Last));
   end Program_Name;

   --  Method to format the log message for both the console and file.
   function Format_Output (Lvl : Level; Msg : String) return String is
   begin
      return ACF.Image (AC.Clock) & " " & Level'Image (Lvl) & " " & Msg;
   end Format_Output;

   --  Output the log message to the console if it is at or above the log
   --  threshold.
   procedure Output_Stdout (Lvl : Level; Msg : String) is
   begin
      --  Only output to StdOut if the level of the message is
      --  higher than the threshold.
      if Lvl >= Stdout_Threshold then
         TIO.Put_Line (Format_Output (Lvl, Msg));
      end if;
   end Output_Stdout;

   --  Output the log message to the files.
   procedure Output_File (Lvl : Level; Msg : String) is
   begin
      --  Create the files if this is the first call.
      if not Files_Created then
         Create_Files;
      end if;

      --  Start at INFO log and add the Msg then step up to the
      --  next log checking everytime if you are now above
      --  your amount.
      for i in Level'Range loop
         if i <= Lvl then
            TIO.Put_Line (Files (i), Format_Output (Lvl, Msg));
            Stats (i).Lines := Stats (i).Lines + 1;
         end if;
      end loop;

      --  If the message recieved is FATAL throw a error to
      --  terminate the program.
      if Lvl = FATAL then
         raise Program_Error with "FATAL ERROR OCCURED";
      end if;
   end Output_File;

   --  Common output method that logs based on the log location.
   --  Lock surrounding.
   procedure Output (Lvl : Level; Msg : String) is
   begin
      Lock.Seize;
      case Log_Location is
         when NONE =>
            null;
         when STDOUT =>
            Output_Stdout (Lvl, Msg);
         when FILE =>
            Output_File (Lvl, Msg);
         when BOTH =>
            Output_Stdout (Lvl, Msg);
            Output_File (Lvl, Msg);
      end case;
      Lock.Release;
   end Output;

   ---------------------------------------------------------------------
   --  Public methods
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   --  Logging
   ---------------------------------------------------------------------

   procedure Info (Msg : String) is
   begin
      Output (INFO, Msg);
   end Info;

   procedure Warn (Msg : String) is
   begin
      Output (WARN, Msg);
   end Warn;

   procedure Error (Msg : String) is
   begin
      Output (ERROR, Msg);
   end Error;

   procedure Fatal (Msg : String) is
   begin
      Output (FATAL, Msg);
   end Fatal;

   ---------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------

   procedure Set_LogTo (Output : LogTo) is
   begin
      Log_Location := Output;
   end Set_LogTo;

   procedure Set_LogTo (Output : String) is
   begin
      Set_LogTo (LogTo'Value (Output));
   end Set_LogTo;

   procedure Set_Stdout_Threshold (Lvl : Level) is
   begin
      Stdout_Threshold := Lvl;
   end Set_Stdout_Threshold;

   procedure Set_Stdout_Threshold (Lvl : String) is
   begin
      Set_Stdout_Threshold (Level'Value (Lvl));
   end Set_Stdout_Threshold;

   procedure Set_File_Path (Path : String) is
   begin
      File_Location := ASU.To_Unbounded_String (Path);
      Files_Location_Set := True;
   end Set_File_Path;

   ---------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------

   function Lines (Lvl : Level) return Natural is
   begin
      return Stats (Lvl).Lines;
   end Lines;

end Alog;
