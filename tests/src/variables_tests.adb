-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

with AUnit.Assertions; use AUnit.Assertions;

with Simple_Logging;

with Mold;    use Mold;
with Support; use Support;

package body Variables_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Variables_Test_Case) return Test_String is
     (Format ("Variables Tests"));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Variables_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_No_Substitution'Access, "No Substitutions Expected");
      Register_Routine
        (T, Test_Basic_Substitution'Access, "Basic Substitutions");
   end Register_Tests;

   --------------------------
   -- Test_No_Substitution --
   --------------------------

   procedure Test_No_Substitution (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  source => no-vars.txt.mold, definitions => empty.toml
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/empty.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 0,
         Definitions => 0,
         Variables   => 0,
         Undefined   => 0,
         Replaced    => 0,
         Ignored     => 0,
         Emptied     => 0,
         Warnings    => 0,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  source => foo.txt.mold, definitions => empty.toml
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/foo.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/empty.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 0,
         Definitions => 0,
         Variables   => 9,
         Undefined   => 9,
         Replaced    => 0,
         Ignored     => 9,
         Emptied     => 0,
         Warnings    => 9,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  source => foo.txt.mold, definitions => bar.toml
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/foo.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/bar.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 1,
         Definitions => 1,
         Variables   => 9,
         Undefined   => 9,
         Replaced    => 0,
         Ignored     => 9,
         Emptied     => 0,
         Warnings    => 9,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

   end Test_No_Substitution;

   -----------------------------
   -- Test_Basic_Substitution --
   -----------------------------

   procedure Test_Basic_Substitution (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  source => foo.txt.mold, definitions => foo.toml
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/foo.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/foo.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 1,
         Definitions => 1,
         Variables   => 9,
         Undefined   => 0,
         Replaced    => 9,
         Ignored     => 0,
         Emptied     => 0,
         Warnings    => 0,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  source => foo-bar.txt.mold, definitions => foo.toml
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/foo-bar.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/foo.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 0,
         Definitions => 1,
         Variables   => 4,
         Undefined   => 2,
         Replaced    => 2,
         Ignored     => 2,
         Emptied     => 0,
         Warnings    => 2,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  source => foo-bar.txt.mold, definitions => foo-bar.toml
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/foo-bar.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/foo-bar.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 1,
         Definitions => 2,
         Variables   => 4,
         Undefined   => 0,
         Replaced    => 4,
         Ignored     => 0,
         Emptied     => 0,
         Warnings    => 0,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

   end Test_Basic_Substitution;

end Variables_Tests;
