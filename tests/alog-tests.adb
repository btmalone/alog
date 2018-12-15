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

with AUnit.Assertions; use AUnit.Assertions;

package body Alog.Tests is

   procedure Test_Logs_Initialize (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Logs_Coutning (T : in out Test_Cases.Test_Case'Class);
   --  Cant do.
   --  procedure Test_Fatal_Raises_Error
   --    (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Program_Name (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Program_Time (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_File_Path (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_File_Path_No_Access
      (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_LogTo (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_LogTo_String (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_Stdout_Threshold (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Set_Stdout_Threshold_String
      (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Vlog (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Vmodule_Setup (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Format_Module (T : in out Test_Cases.Test_Case'Class);

   --  Logs start at zero.
   procedure Test_Logs_Initialize (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Expected : constant Natural := 0;
      Actual : Natural;
   begin
      Actual := Lines (INFO);
      Assert (Expected = Actual, "Info log is not empty");
      Actual := Lines (WARN);
      Assert (Expected = Actual, "Warn log is not empty");
      Actual := Lines (ERROR);
      Assert (Expected = Actual, "Error log is not empty");
      Actual := Lines (FATAL);
      Assert (Expected = Actual, "Fatal log is not empty");
   end Test_Logs_Initialize;

   --  Test the logs increment correctly.
   --  Logs are written to the a file if message is at that level or above
   --  i.e. WARNs are written to both WARN and INFO
   procedure Test_Logs_Coutning (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Actual : Natural;
   begin
      Info ("foo bar buzz");
      Actual := Lines (INFO);
      Assert (1 = Actual, "Log increments incorrectly");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Actual := Lines (WARN);
      Assert (4 = Actual, "Log increments incorrectly");
      --  INFO Log also stores the WARN logs
      Actual := Lines (INFO);
      Assert (5 = Actual, "Log increments incorrectly");
   end Test_Logs_Coutning;

   --  Get the name of the currently running program. Is used exclusively
   --  with ACL.Command_Name.
   --  ACL.Command_Name returns a string with the name as well as the file
   --  path. Trim the file path so it can be used in the log file name.
   --  e.g. ./foo -> foo
   --       ../../../bar -> bar
   --       src/test/buzz -> buzz
   procedure Test_Program_Name (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert ("foo" = Program_Name ("./foo"),
         "Program name parsed incorrectly");
      Assert ("bar" = Program_Name ("../../../bar"),
         "Program name parsed incorrectly");
      Assert ("buzz" = Program_Name ("src/test/buzz"),
         "Program name parsed incorrectly");
   end Test_Program_Name;

   --  Get the time of the currently running program. Is used exclusively
   --  with ACF.Image (AC.Clock).
   --  ACF.Image returns a string with dashes, semicolons and a space. Remove
   --  the characters so it can be used in the log file name.
   --  e.g 2018-12-01 08:08:20 -> 20181201.080820
   procedure Test_Program_Time (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert ("20181201.080820" = Program_Time ("2018-12-01 08:08:20"),
         "Program name parsed incorrectly");
   end Test_Program_Time;

   procedure Test_Set_File_Path (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  This causes it to hand on follow on test cases.
      --  Files_Created := False;
      Set_File_Path ("../");
      --  Info ("foo bar buzz");
   end Test_Set_File_Path;

   procedure Test_Set_File_Path_No_Access
      (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Files_Created := False;
      Set_File_Path ("/");
      --  Files_Created := False;
      --  Set_File_Path ("made up ath name");
   end Test_Set_File_Path_No_Access;

   procedure Test_Set_LogTo (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Actual : Natural;
   begin
      --  Stop logging to the files, and verify the incrementing stops
      Set_LogTo (NONE);
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Actual := Lines (WARN);
      Assert (0 = Actual, "Log increments incorrectly");
      Set_LogTo (FILE);
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Actual := Lines (WARN);
      Assert (2 = Actual, "Log increments incorrectly");
   end Test_Set_LogTo;

   procedure Test_Set_LogTo_String
      (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Actual : Natural;
   begin
      --  Stop logging to the files, and verify the incrementing stops
      Set_LogTo ("NONE");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Actual := Lines (WARN);
      Assert (0 = Actual, "Log increments incorrectly");
      Set_LogTo ("FILE");
      Warn ("foo bar buzz");
      Warn ("foo bar buzz");
      Actual := Lines (WARN);
      Assert (2 = Actual, "Log increments incorrectly");
   end Test_Set_LogTo_String;

   procedure Test_Set_Stdout_Threshold
      (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Set_Stdout_Threshold (INFO);
      Info ("foo bar buzz");
   end Test_Set_Stdout_Threshold;

   procedure Test_Set_Stdout_Threshold_String
      (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Set_Stdout_Threshold ("INFO");
      Info ("foo bar buzz");
   end Test_Set_Stdout_Threshold_String;

   procedure Test_Vlog (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Vlog (0, "foo bar buzz");
   end Test_Vlog;

   procedure Test_Vmodule_Setup (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Vmodule_Setup ("foo=0");
      Modules_Map.Clear;
      Vmodule_Setup ("fooa=22,barbuzz=2");
      Modules_Map.Clear;
      Vmodule_Setup ("alog-tests=2,asdf=2");
      Vlog (3, "Not Logged");
      Vlog (2, "Logged");
   end Test_Vmodule_Setup;

   procedure Test_Format_Module (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert ("test-file" = Format_Module ("test-file.adb:100"), "asdf");
   end Test_Format_Module;

   --  Reset the Log stats so we can start fresh in every test case.
   procedure Set_Up (T : in out Alog_Test) is
      pragma Unreferenced (T);
   begin
      for Lvl in Level'Range loop
         Stats (Lvl).Lines := 0;
      end loop;
      Modules_Map.Clear;
   end Set_Up;

   --  Register test routines to call
   procedure Register_Tests (T : in out Alog_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      --  Repeat for each test routine:
      Register_Routine (T, Test_Logs_Initialize'Access, "Test Initialization");
      Register_Routine (T, Test_Logs_Coutning'Access, "Test Log Counting");
      Register_Routine (T, Test_Program_Name'Access, "Test Program Name");
      Register_Routine (T, Test_Program_Time'Access, "Test Program Time");
      Register_Routine (T, Test_Set_File_Path'Access, "Test Set File Path");
      Register_Routine (T, Test_Set_File_Path_No_Access'Access,
         "Test Set File Path No Access");
      Register_Routine (T, Test_Set_LogTo'Access, "Test Set LogTo");
      Register_Routine (T, Test_Set_LogTo_String'Access,
         "Test Set LogTo String");
      Register_Routine (T, Test_Set_Stdout_Threshold'Access,
         "Test Set Stdout Threshold");
      Register_Routine (T, Test_Set_Stdout_Threshold_String'Access,
         "Test Set Stdout Threshold String");
      Register_Routine (T, Test_Vlog'Access, "Test Vlog");
      Register_Routine (T, Test_Vmodule_Setup'Access, "Test Vmodule Setup");
      Register_Routine (T, Test_Format_Module'Access, "Test Format Module");
   end Register_Tests;

   --  Identifier of test case
   function Name (T : Alog_Test) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format ("Alog Tests");
   end Name;

end Alog.Tests;