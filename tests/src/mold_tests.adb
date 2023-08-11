-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Containers;
with Ada.Text_IO;

with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Test_Results;

with Libmold_Tests_Config;
with Lib_Mold_Test_Suite;
with Lib_Mold;

----------------
-- Mold_Tests --
----------------

procedure Mold_Tests is

   package Mold renames Lib_Mold;

   procedure Run is new AUnit.Run.Test_Runner_With_Results
     (Lib_Mold_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Results  : AUnit.Test_Results.Result;

   use Libmold_Tests_Config;
begin
   Ada.Text_IO.Put_Line
     ("Tests for " & Mold.Name & " version " & Mold.Version);

   pragma Warnings (Off);
   Reporter.Set_Use_ANSI_Colors
     (Libmold_Tests_Config.Build_Profile = Libmold_Tests_Config.development);
   pragma Warnings (On);

   Run (Reporter, Results);

   if not Results.Successful then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Mold_Tests;
