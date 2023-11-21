-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Variables_Tests is

   type Variables_Test_Case is new Test_Case with null record;

   overriding function Name (T : Variables_Test_Case) return Message_String;

   overriding procedure Register_Tests (T : in out Variables_Test_Case);

   procedure Test_Variables_Definition (T : in out Test_Case'Class);
   procedure Test_No_Substitution (T : in out Test_Case'Class);
   procedure Test_Basic_Substitution (T : in out Test_Case'Class);
   procedure Test_Modal_Substitution (T : in out Test_Case'Class);
   procedure Test_Multiline (T : in out Test_Case'Class);

end Variables_Tests;
