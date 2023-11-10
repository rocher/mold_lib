-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

with Mold_Lib; use Mold_Lib;
with Support;  use Support;

package body Rename_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Rename_Test_Case) return Test_String is
     (Format ("File Rename Tests"));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Rename_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_No_Renaming'Access, "No Files Renamed");
      Register_Routine (T, Test_Basic_Renaming'Access, "Basic File Renaming");
   end Register_Tests;

   ----------------------
   -- Test_No_Renaming --
   ----------------------

   procedure Test_No_Renaming (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- file renaming disabled ----------------------------------------
      Settings.Replacement_In_Filenames := False;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/foo.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   => 1,
         Variables_Defined => 1,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__.txt", "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- file renaming disabled ----------------------------------------
      Settings.Replacement_In_Filenames := False;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/foo-bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   => 1,
         Variables_Defined => 2,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__-__bar__.txt",
         "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- variable in source file name is undefined ---------------------
      Settings.Replacement_In_Filenames := True;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   => 1,
         Files_Overwritten => 1,
         Variables_Defined => 1,
         Warnings          => 1,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__.txt", "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- variable in source file name is undefined ---------------------
      Settings.Replacement_In_Filenames := True;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   =>  1,
         Files_Overwritten =>  1,
         Variables_Defined => 26,
         Warnings          =>  2,
         others            =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__.txt", "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");
   end Test_No_Renaming;

   -------------------------
   -- Test_Basic_Renaming --
   -------------------------

   procedure Test_Basic_Renaming (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      Settings.Replacement_In_Filenames := True;

      --  ----- one variable replaced -----------------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/foo.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   => 1,
         Files_Renamed     => 1,
         Variables_Defined => 1,
         Warnings          => 1,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-foo-__bar__.txt",
         "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- one variable replaced -----------------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   => 1,
         Files_Renamed     => 1,
         Variables_Defined => 1,
         Warnings          => 1,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-__foo__-bar.txt",
         "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- two variables replaced ----------------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/no-vars-__foo__-__bar__.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/foo-bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed   => 1,
         Files_Renamed     => 1,
         Variables_Defined => 2,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars-foo-bar.txt", "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");
   end Test_Basic_Renaming;

end Rename_Tests;
