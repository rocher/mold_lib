-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Lib_Mold;    use Lib_Mold;
with Support; use Support;

package body Rename_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Files_Test_Case) return Test_String is
     (Format ("File Rename Tests"));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Files_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_No_Renaming'Access, "No Files Renamed");
      Register_Routine (T, Test_Basic_Renaming'Access, "Basic File Renaming");
   end Register_Tests;

   ----------------------
   -- Test_No_Renaming --
   ----------------------

   procedure Test_No_Renaming (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin
      --  ----- file renaming disabled ----------------------------------------
      Settings.Rename_Source := False;
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/foo.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 0,
         Definitions => 1,
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
        ("suite/tmp/no-vars-__foo__.txt", "7ef8e151c0fde9d5fef738709a321300");

      --  ----- file renaming disabled ----------------------------------------
      Settings.Rename_Source := False;
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/foo-bar.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 0,
         Definitions => 2,
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
        ("suite/tmp/no-vars-__foo__-__bar__.txt",
         "7ef8e151c0fde9d5fef738709a321300");

      --  ----- variable in source file name is undefined ---------------------
      Settings.Rename_Source := True;
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/bar.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 0,
         Overwritten => 1,
         Definitions => 1,
         Variables   => 0,
         Undefined   => 0,
         Replaced    => 0,
         Ignored     => 0,
         Emptied     => 0,
         Warnings    => 1,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__.txt", "7ef8e151c0fde9d5fef738709a321300");

      --  ----- variable in source file name is undefined ---------------------
      Settings.Rename_Source := True;
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       =>  1,
         Renamed     =>  0,
         Overwritten =>  1,
         Definitions => 26,
         Variables   =>  0,
         Undefined   =>  0,
         Replaced    =>  0,
         Ignored     =>  0,
         Emptied     =>  0,
         Warnings    =>  2,
         Mold.Errors =>  0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__.txt", "7ef8e151c0fde9d5fef738709a321300");

   end Test_No_Renaming;

   -------------------------
   -- Test_Basic_Renaming --
   -------------------------

   procedure Test_Basic_Renaming (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin

      Settings.Rename_Source := True;

      --  ----- one variable replaced -----------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/foo.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 1,
         Overwritten => 0,
         Definitions => 1,
         Variables   => 0,
         Undefined   => 0,
         Replaced    => 0,
         Ignored     => 0,
         Emptied     => 0,
         Warnings    => 1,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/no-vars-foo-__bar__.txt",
         "7ef8e151c0fde9d5fef738709a321300");

      --  ----- one variable replaced -----------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/bar.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 1,
         Overwritten => 0,
         Definitions => 1,
         Variables   => 0,
         Undefined   => 0,
         Replaced    => 0,
         Ignored     => 0,
         Emptied     => 0,
         Warnings    => 1,
         Mold.Errors => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__-bar.txt",
         "7ef8e151c0fde9d5fef738709a321300");

      --  ----- two variables replaced ----------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/foo-bar.toml",
         Results     => Results'Unchecked_Access
      );
      Expected := [
         Files       => 1,
         Renamed     => 1,
         Overwritten => 0,
         Definitions => 2,
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
        ("suite/tmp/no-vars-foo-bar.txt", "7ef8e151c0fde9d5fef738709a321300");

   end Test_Basic_Renaming;

end Rename_Tests;
