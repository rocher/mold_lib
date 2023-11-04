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

package body Filters_Tests is

   function Replace_By_Slash (S : String) return String;
   function Double_Slash (S : String) return String;

   ----------
   -- Name --
   ----------

   overriding function Name (T : Filters_Test_Case) return Test_String is
     (Format ("Filters Tests    "));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Filters_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Predefined_Filters'Access, "Predefined Filters");
      Register_Routine (T, Test_Custom_Filters'Access, "Custom Filters");
      Register_Routine (T, Test_Invalid_Filters'Access, "Invalid Filters");
   end Register_Tests;

   -----------------------------
   -- Test_Predefined_Filters --
   -----------------------------

   procedure Test_Predefined_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Settings : Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- variable substitution with text filters: errors as warnings ---
      Settings.Undefined_Action := Ignore;
      Settings.Undefined_Alert  := Warning;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/predefined-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/predefined-filters.toml",
         Filters    => null,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Variables_Defined  => 12,
         Variables_Found    => 59,
         Variables_Replaced => 59,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/predefined-filters.txt",
         "353c38629937471f0ab7fd35f3fbbf57",
         "add6d37be204a3e0056be4ede8a0d6f2");

   end Test_Predefined_Filters;

   -------------------------
   -- Test_Custom_Filters --
   -------------------------

   procedure Test_Custom_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Settings : Mold.Settings_Type         := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      --!pp off
      Filters  : aliased Mold.Filters_Array := [
        0 => Replace_By_Slash'Access,
        1 => Double_Slash'Access,
        8 => Double_Slash'Access,
        9 => Replace_By_Slash'Access,
        others => null
      ];
      --!pp on
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- variable substitution with custom text filters ----------------
      Settings.Undefined_Action := Ignore;
      Settings.Undefined_Alert  := Warning;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/custom-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/custom-filters.toml",
         Filters    => Filters'Unchecked_Access,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Variables_Defined  =>  2,
         Variables_Found    => 12,
         Variables_Replaced => 12,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/custom-filters.txt", "f92b78616d8d697866bdf9ba1ffe88f1",
         "9f0424e0e73d383ee16f8e2fbbac73a3");
   end Test_Custom_Filters;

   --------------------------
   -- Test_Invalid_Filters --
   --------------------------

   procedure Test_Invalid_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      --!pp off
      Filters  : aliased Mold.Filters_Array := [
        3 => Replace_By_Slash'Access,
        4 => Double_Slash'Access,
        8 => Double_Slash'Access,
        9 => Replace_By_Slash'Access,
        others => null
      ];
      --!pp on
   begin
      --  ----- variable substitution with text filters: ignore and warn ------
      Settings.Undefined_Action := Ignore;
      Settings.Undefined_Alert  := Warning;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/invalid-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/custom-filters.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Variables_Defined  =>  2,
         Variables_Found    => 20,
         Variables_Replaced =>  0,
         Variables_Ignored  => 20,
         Warnings           => 20,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/invalid-filters.txt", "835fc3f56a9e2060cdb1d4ac0a75c401",
         "4648a17d49139ef4ea9249711c8455a6");

      --  ----- variable substitution with text filters: empty and warn -------
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Action            := Empty;
      Settings.Undefined_Alert             := Warning;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/invalid-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/custom-filters.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Files_Overwritten  =>  1,
         Variables_Defined  =>  2,
         Variables_Found    => 20,
         Variables_Replaced =>  0,
         Variables_Ignored  =>  0,
         Variables_Emptied  => 20,
         Warnings           => 20,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/invalid-filters.txt", "cc2e6aa0f37953e2f79eeb635da74c39",
         "d6b7e1fbd7fa6eba943b417e55f56271");

      --  ----- undefined custom text filter: ignore and warn -----------------
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Action            := Ignore;
      Settings.Undefined_Alert             := Warning;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/custom-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/custom-filters.toml",
         Filters    => Filters'Unchecked_Access,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Files_Overwritten  =>  1,
         Variables_Defined  =>  2,
         Variables_Found    => 12,
         Variables_Replaced =>  8,
         Variables_Ignored  =>  4,
         Warnings           =>  4,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/custom-filters.txt", "b7501d2677f79ecd9f5969dee6574bf3",
         "a8ab488e7078d006c15347c2c17143c0");

      --  ----- undefined custom text filter: empty and error -----------------
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Action            := Empty;
      Settings.Undefined_Alert             := Error;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/custom-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/custom-filters.toml",
         Filters    => Filters'Unchecked_Access,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Files_Overwritten  =>  1,
         Variables_Defined  =>  2,
         Variables_Found    => 12,
         Variables_Replaced =>  8,
         Variables_Ignored  =>  0,
         Variables_Emptied  =>  4,
         Warnings           =>  0,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/custom-filters.txt", "6e727f4e4fb46327223feae96e6f74ca",
         "72a028acdb8b8f85f64de1c5ce776a0d");
   end Test_Invalid_Filters;

   ----------------------
   -- Replace_By_Slash --
   ----------------------

   function Replace_By_Slash (S : String) return String is
   begin
      return Dash : String (1 .. S'Length) do
         for D of Dash loop
            D := '/';
         end loop;
      end return;
   end Replace_By_Slash;

   ------------------
   -- Double_Slash --
   ------------------

   function Double_Slash (S : String) return String is
   begin
      return Dash : String (1 .. S'Length * 2) do
         for D of Dash loop
            D := '/';
         end loop;
      end return;
   end Double_Slash;

end Filters_Tests;
