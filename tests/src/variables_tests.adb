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

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

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
      Register_Routine
        (T, Test_Modal_Substitution'Access, "Modal Substitutions");
   end Register_Tests;

   --------------------------
   -- Test_No_Substitution --
   --------------------------

   procedure Test_No_Substitution (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  ----- no variables in the source file -------------------------------
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
      Check_MD5_Digest
        ("suite/tmp/no-vars.txt", "7ef8e151c0fde9d5fef738709a321300");

      --  ----- empty definitions file ----------------------------------------
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
      Check_MD5_Digest
        ("suite/tmp/foo.txt", "4c179dd0c4cc0c668539a25435286258");

      --  ----- no variable can be replaced -----------------------------------
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
      Check_MD5_Digest
        ("suite/tmp/foo.txt", "4c179dd0c4cc0c668539a25435286258");

   end Test_No_Substitution;

   -----------------------------
   -- Test_Basic_Substitution --
   -----------------------------

   procedure Test_Basic_Substitution (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  ----- variable replaced ---------------------------------------------
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
      Check_MD5_Digest
        ("suite/tmp/foo.txt", "3d22c1e66750c3e7925e643cfbe9e327");

      --  ----- four variables, two are replaced ------------------------------
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
      Check_MD5_Digest
        ("suite/tmp/foo-bar.txt", "9fe90f7706a6c0de1155e8e340fafed7");

      --  ----- all variables replaced ----------------------------------------
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
      Check_MD5_Digest
        ("suite/tmp/foo-bar.txt", "5b6c9393c2233d09b1517bc8c3ca9de1");

   end Test_Basic_Substitution;

   -----------------------------
   -- Test_Modal_Substitution --
   -----------------------------

   procedure Test_Modal_Substitution (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin

      --  ----- all variables replaced ----------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       =>    1,
         Renamed     =>    0,
         Overwritten =>    0,
         Definitions =>   26,
         Variables   => 2118,
         Undefined   =>    0,
         Replaced    => 2118,
         Ignored     =>    0,
         Emptied     =>    0,
         Warnings    =>    0,
         Mold.Errors =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "ff416bfec859c59a3834c46d60250e25");

      --  ----- no optional variables defined ---------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum_no-opts1.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       =>    1,
         Renamed     =>    0,
         Overwritten =>    1,
         Definitions =>   23,
         Variables   => 2118,
         Undefined   =>  291,
         Replaced    => 1827,
         Ignored     =>    0,
         Emptied     =>  291,
         Warnings    =>    0,
         Mold.Errors =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "fee4ce163f4e85103e42ab27a49ee381");

      --  ----- some optional variables defined -------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum_no-opts2.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       =>    1,
         Renamed     =>    0,
         Overwritten =>    1,
         Definitions =>   24,
         Variables   => 2118,
         Undefined   =>  176,
         Replaced    => 1942,
         Ignored     =>    0,
         Emptied     =>  176,
         Warnings    =>    0,
         Mold.Errors =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "1ed55361c952f1e572a156c07a3c2f3d");

   end Test_Modal_Substitution;

end Variables_Tests;
