-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
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

with Mold_Test_Suite;
with Libmold_Tests_Config; use Libmold_Tests_Config;
with Mold;

procedure Mold_Tests is

   procedure Run is new AUnit.Run.Test_Runner_With_Results
     (Mold_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Results  : AUnit.Test_Results.Result;

begin
   --  Ada.Text_IO.Put_Line
   --    ("   " & Euler_Tools.Library_Name & " = " & Euler_Tools.Library_Version);

   pragma Warnings (Off);
   Reporter.Set_Use_ANSI_Colors (Build_Profile = development);
   pragma Warnings (On);

   Run (Reporter, Results);

   if not Results.Successful then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Mold_Tests;
