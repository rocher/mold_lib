-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Rename_Tests;    use Rename_Tests;
with Variables_Tests; use Variables_Tests;
with Inclusion_Tests; use Inclusion_Tests;
with Directory_Tests; use Directory_Tests;

package body Mold_Test_Suite is

   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Files_Test     : aliased Files_Test_Case;
   Variables_Test : aliased Variables_Test_Case;
   Inclusion_Test : aliased Inclusion_Test_Case;
   Directory_Test : aliased Directory_Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin

      Add_Test (Result'Access, Variables_Test'Access);
      Add_Test (Result'Access, Files_Test'Access);
      Add_Test (Result'Access, Inclusion_Test'Access);
      Add_Test (Result'Access, Directory_Test'Access);

      return Result'Access;
   end Suite;

end Mold_Test_Suite;
