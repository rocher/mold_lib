-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Exception_Traces;

with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Test_Results;

with Mold_Lib_Tests_Config;
with Mold_Lib_Test_Suite;
with Mold_Lib;

with Simple_Logging;

procedure Mold_Lib_Tests is

   procedure Run is new AUnit.Run.Test_Runner_With_Results
     (Mold_Lib_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Results  : AUnit.Test_Results.Result;

   use Mold_Lib_Tests_Config;
begin
   Simple_Logging.Level := Simple_Logging.Debug;

   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

   Ada.Text_IO.Put_Line
     ("Tests for " & Mold_Lib.Name & " version " & Mold_Lib.Version);

   pragma Warnings (Off);
   Reporter.Set_Use_ANSI_Colors
     (Mold_Lib_Tests_Config.Build_Profile = Mold_Lib_Tests_Config.development);
   pragma Warnings (On);

   Run (Reporter, Results);

   if not Results.Successful then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Mold_Lib_Tests;
