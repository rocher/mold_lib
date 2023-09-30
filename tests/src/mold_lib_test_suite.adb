-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Variables_Tests; use Variables_Tests;
with Rename_Tests;    use Rename_Tests;
with Inclusion_Tests; use Inclusion_Tests;
with Directory_Tests; use Directory_Tests;
with Filters_Tests; use Filters_Tests;

package body Mold_Lib_Test_Suite is

   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Variables_Test : aliased Variables_Test_Case;
   Rename_Test    : aliased Rename_Test_Case;
   Inclusion_Test : aliased Inclusion_Test_Case;
   Directory_Test : aliased Directory_Test_Case;
   Filters_Test   : aliased Filters_Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Add_Test (Result'Access, Variables_Test'Access);
      Add_Test (Result'Access, Rename_Test'Access);
      Add_Test (Result'Access, Inclusion_Test'Access);
      Add_Test (Result'Access, Directory_Test'Access);
      Add_Test (Result'Access, Filters_Test'Access);

      return Result'Access;
   end Suite;

end Mold_Lib_Test_Suite;
