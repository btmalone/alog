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

package body Alog.Test is
   use Assertions;

   procedure BLAH (T : in out Test_Cases.Test_Case'Class);

   procedure BLAH (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      C : Natural;
   begin
      C := Lines (ERROR);
      Assert (C = 0, "Incorrect result after addition");
   end BLAH;

   --  Register test routines to call
   procedure Register_Tests (T : in out Alog_Test) is
      use Test_Cases.Registration;
   begin
      --  Repeat for each test routine:
      Register_Routine (T, BLAH'Access, "Test Addition");
   end Register_Tests;

   function Name (T : Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Test Alog");
   end Name;

end Alog.Test;
