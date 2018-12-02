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
with Alog;

package body Alog_Tests is

   procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Expected : constant Natural := 0;
      Actual : Natural;
   begin
      Actual := Alog.Lines (Alog.INFO);
      Assert (Expected = Actual, "Addition is incorrect");
   end Test_Simple_Add;

   --  Register test routines to call
   procedure Register_Tests (T : in out Alog_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      --  Repeat for each test routine:
      Register_Routine (T, Test_Simple_Add'Access, "Test Addition");
   end Register_Tests;

   --  Identifier of test case
   function Name (T : Alog_Test) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format ("Alog Tests");
   end Name;

end Alog_Tests;