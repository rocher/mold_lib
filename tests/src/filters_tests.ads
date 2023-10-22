-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Filters_Tests is

   type Filters_Test_Case is new Test_Case with null record;

   overriding function Name (T : Filters_Test_Case) return Message_String;

   overriding procedure Register_Tests (T : in out Filters_Test_Case);

   procedure Test_Predefined_Filters (T : in out Test_Case'Class);
   procedure Test_Custom_Filters (T : in out Test_Case'Class);
   procedure Test_Invalid_Filters (T : in out Test_Case'Class);

end Filters_Tests;
