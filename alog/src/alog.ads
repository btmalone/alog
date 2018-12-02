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

package Alog is

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
   procedure Info (msg : String);
   procedure Warn (msg : String);
   procedure Error (msg : String);
   procedure Fatal (msg : String);

   ---------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------

   --  Set where the logs saved. By default set to BOTH.
   procedure Set_LogTo (output : LogTo);

   --  Helper method to you can pass a String representation of LogTo.
   procedure Set_LogTo (output : String);

   --  Set what the threshold level of stdout will be.
   procedure Set_Stdout_Threshold (lvl : Level);

   --  Helper method to you can pass a String representation of Level.
   procedure Set_Stdout_Threshold (lvl : String);

   --  Set where the log files shoudl be saved.
   procedure Set_File_Path (path : String);

   ---------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------

   --  Return the number of lines written to a specified log file.
   function Lines (lvl : Level) return Natural;

end Alog;