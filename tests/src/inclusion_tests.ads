-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Inclusion_Tests is

   type Inclusion_Test_Case is new Test_Case with null record;

   overriding function Name (T : Inclusion_Test_Case) return Message_String;

   overriding procedure Register_Tests (T : in out Inclusion_Test_Case);

   procedure Test_Recursive_Inclusion (T : in out Test_Case'Class);
   procedure Test_Inclusion (T : in out Test_Case'Class);

end Inclusion_Tests;
