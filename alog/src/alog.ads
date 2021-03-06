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

with GNAT.Source_Info;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Alog is

   package ASU renames Ada.Strings.Unbounded;

   --  Four levels of loggging.
   type Level is (
      INFO,
      WARN,
      ERROR,
      FATAL
   );

   --  Type representing where things are logged to.
   type LogTo is (
      NONE,
      STDOUT,
      FILE,
      BOTH
   );

   ---------------------------------------------------------------------
   --  Logging
   ---------------------------------------------------------------------

   --  Methods for each log message level.
   procedure Info (Msg : String);
   procedure Warn (Msg : String);
   procedure Error (Msg : String);
   procedure Fatal (Msg : String);

   procedure Vlog (Lvl : Natural;
                  Msg  : String;
                  Source : String := GNAT.Source_Info.Source_Location);

   ---------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------

   --  Set where the logs saved. By default set to BOTH.
   procedure Set_LogTo (Output : LogTo);

   --  Helper method to you can pass a String representation of LogTo.
   procedure Set_LogTo (Output : String);

   --  Set what the threshold level of stdout will be.
   procedure Set_Stdout_Threshold (Lvl : Level);

   --  Helper method to you can pass a String representation of Level.
   procedure Set_Stdout_Threshold (Lvl : String);

   --  Set where the log files shoudl be saved.
   procedure Set_File_Path (Path : String);

   --  Set the Vlog Threshold.
   procedure Set_Vlog_Threshold (Lvl : Natural);

   --  blah
   procedure Set_Vlog_Modules (Mods : String);

   ---------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------

   --  Return the number of lines written to a specified log file.
   function Lines (Lvl : Level) return Natural;

private
   --  Record of statistics about the log levels.
   type Level_Stats is record
      Lines : Natural := 0;
   end record;

   --  Array of level stats for each log level.
   type Log_Stats is array (Level) of Level_Stats;
   Stats : Log_Stats;

   function Program_Name (Cmd : String) return String;
   function Program_Time (Time : String) return String;
   function Format_Module (Source : String) return String;

   procedure Vmodule_Setup (Mods : String);

   Files_Created : Boolean := False;
   Files_Location_Set : Boolean := False;

   function Equivalent_Strings (Left, Right : ASU.Unbounded_String)
      return Boolean;

   package Module is new Ada.Containers.Hashed_Maps
     (Key_Type => ASU.Unbounded_String,
      Element_Type => Natural,
      Hash => ASU.Hash,
      Equivalent_Keys => Equivalent_Strings);

   Modules_Map : Module.Map;

end Alog;
