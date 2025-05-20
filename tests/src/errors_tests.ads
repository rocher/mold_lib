-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Errors_Tests is

   type Errors_Test_Case is new Test_Case with null record;

   overriding function Name (T : Errors_Test_Case) return Message_String;

   overriding procedure Register_Tests (T : in out Errors_Test_Case);

   procedure Variable_Errors (T : in out Test_Case'Class);
   procedure File_Errors (T : in out Test_Case'Class);
   procedure Directory_Errors (T : in out Test_Case'Class);
   procedure Validations_Errors (T : in out Test_Case'Class);

end Errors_Tests;
