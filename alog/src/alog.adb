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
------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Text_IO;

package body Alog is

   package AC  renames Ada.Calendar;
   package ACF renames Ada.Calendar.Formatting;
   package ACL renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package AEV renames Ada.Environment_Variables;

   ---------------------------------------------------------------------
   --  Private method declarations
   ---------------------------------------------------------------------

   procedure Create_Files;

   function Format_Output (lvl : Level; msg : String) return String;
   procedure Output (lvl : Level; msg : String);
   procedure Output_Stdout (lvl : Level; msg : String);
   procedure Output_File (lvl : Level; msg : String);

   function Program_Name return String;
   function Program_Time return String;

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

   --  function Get_Hostname (name : access C.int; size : C.int)
   --  return Integer;
   --  pragma Import
   --    (Convention    => C,
   --     Entity        => Get_Hostname,
   --     External_Name => "gethostname");

   ---------------------------------------------------------------------
   --  Package variables
   ---------------------------------------------------------------------

   --  By default only log to the console if the log message is an Error
   --  or greater.
   Stdout_Threshold : Level := ERROR;

   --  By default write both to the log files and the console.
   Log_Location : LogTo := BOTH;

   --  By default write all log files to the tmp directory.
   File_Location : String := "/tmp/";

   Files_Created : Boolean := False;

   --  Array of a file handler for each log level.
   type Log_Files is array (Level) of TIO.File_Type;
   Files : Log_Files;

   --  Record of statistics about the log levels.
   type Level_Stats is record
      Lines : Natural := 0;
   end record;

   --  Array of level stats for each log level.
   type Log_Stats is array (Level) of Level_Stats;
   Stats : Log_Stats;

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
      cmd  : constant String := Program_Name;
      host : constant String := "host"; --  ASU.To_String (Get_Hostname);
      user : constant String := AEV.Value ("USER");
      time : constant String := Program_Time;
      pid  : constant String := Integer'Image (Get_PID);
      prefix : constant String := File_Location & cmd & "." &
                           host & "." & user & ".log.";
      suffix : constant String := "." & time & "." & pid;
   begin
      TIO.Put_Line (host);
      for lvl in Files'Range loop
         begin
            TIO.Create (File => Files (lvl),
                        Mode => TIO.Out_File,
                        Name => prefix & Level'Image (lvl) & suffix);
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
   function Program_Time return String is
      time : constant String := ACF.Image (AC.Clock);
      str : String (1 .. 15);
      pos : Natural := str'First;
   begin
      for i in time'Range loop
         case time (i) is
            when '-' =>
               null;
            when ':' =>
               null;
            when ' ' =>
               str (pos) := '.';
               pos := pos + 1;
            when others =>
               str (pos) := time (i);
               pos := pos + 1;
         end case;
      end loop;
      return str;
   end Program_Time;

   --  Take the time the command name and remove all the dots and slashes
   --  so the program name can be used in the log file name.
   function Program_Name return String is
      cmd : constant String := ACL.Command_Name;
      pos : Integer := 0;
   begin
      for i in cmd'Range loop
         --  Go backwards through the string till a / is found
         if cmd (cmd'Last - i) = '/' then
            pos := i;
            exit;
         end if;
      end loop;
      return (cmd (cmd'Last - pos + 1 .. cmd'Last));
   end Program_Name;

   --  Method to format the log message for both the console and file.
   function Format_Output (lvl : Level; msg : String) return String is
   begin
      return ACF.Image (AC.Clock) & " " & Level'Image (lvl) & " " & msg;
   end Format_Output;

   --  Output the log message to the console if it is at or above the log
   --  threshold.
   procedure Output_Stdout (lvl : Level; msg : String) is
   begin
      --  Only output to StdOut if the level of the message is
      --  higher than the threshold.
      if lvl >= Stdout_Threshold then
         TIO.Put_Line (Format_Output (lvl, msg));
      end if;
   end Output_Stdout;

   --  Output the log message to the files.
   procedure Output_File (lvl : Level; msg : String) is
   begin
      --  Create the files if this is the first call.
      if not Files_Created then
         Create_Files;
      end if;

      --  Start at INFO log and add the msg then step up to the
      --  next log checking everytime if you are now above
      --  your amount.
      for i in Level'Range loop
         if i <= lvl then
            TIO.Put_Line (Files (i), Format_Output (lvl, msg));
            Stats (i).Lines := Stats (i).Lines + 1;
         end if;
      end loop;

      --  If the message recieved is FATAL throw a error to
      --  terminate the program.
      if lvl = FATAL then
         raise Program_Error with "FATAL ERROR OCCURED";
      end if;
   end Output_File;

   --  Common output method that logs based on the log location.
   --  Lock surrounding.
   procedure Output (lvl : Level; msg : String) is
   begin
      Lock.Seize;
      case Log_Location is
         when NONE =>
            null;
         when STDOUT =>
            Output_Stdout (lvl, msg);
         when FILE =>
            Output_File (lvl, msg);
         when BOTH =>
            Output_Stdout (lvl, msg);
            Output_File (lvl, msg);
      end case;
      Lock.Release;
   end Output;

   ---------------------------------------------------------------------
   --  Public methods
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   --  Logging
   ---------------------------------------------------------------------

   procedure Info (msg : String) is
   begin
      Output (INFO, msg);
   end Info;

   procedure Warn (msg : String) is
   begin
      Output (WARN, msg);
   end Warn;

   procedure Error (msg : String) is
   begin
      Output (ERROR, msg);
   end Error;

   procedure Fatal (msg : String) is
   begin
      Output (FATAL, msg);
   end Fatal;

   ---------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------

   procedure Set_LogTo (output : LogTo) is
   begin
      Log_Location := output;
   end Set_LogTo;

   procedure Set_LogTo (output : String) is
   begin
      Set_LogTo (LogTo'Value (output));
   end Set_LogTo;

   procedure Set_Stdout_Threshold (lvl : Level) is
   begin
      Stdout_Threshold := lvl;
   end Set_Stdout_Threshold;

   procedure Set_Stdout_Threshold (lvl : String) is
   begin
      Set_Stdout_Threshold (Level'Value (lvl));
   end Set_Stdout_Threshold;

   procedure Set_File_Path (path : String) is
   begin
      File_Location := path;
   end Set_File_Path;

   ---------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------

   function Lines (lvl : Level) return Natural is
   begin
      return Stats (lvl).Lines;
   end Lines;

end Alog;
