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
      Errors   : Natural;
      Settings : Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- variable substitution with text filters: errors as warnings ---
      Settings.Undefined_Filter_Alert := Warning;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/predefined-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/predefined-filters.toml",
         Filters    => null,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>   1,
         Variables_Defined    =>  12,
         Variables_Found      =>  59,
         Variables_Replaced   =>  59,
         Filters_Found        => 111,
         Filters_Applied      => 110,
         Replacement_Warnings =>   1,
         others               =>   0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);

      Check_MD5_Digest
        ("suite/tmp/predefined-filters.txt",
         "2ecf84f9f0603d4bb949aa20b9ca4a04",
         "363a79ee038c075d5b5313c41cdfd8a8");

      --  ----- variable substitution with text filters: abort on error -------
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Filter_Alert      := Error;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/predefined-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/predefined-filters.toml",
         Filters    => null,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>   1,
         Files_Overwritten    =>   1,
         Variables_Defined    =>  12,
         Variables_Found      =>  59,
         Variables_Replaced   =>  59,
         Filters_Found        => 111,
         Filters_Applied      => 110,
         Replacement_Warnings =>   1,
         others               =>   0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);

      Check_MD5_Digest
        ("suite/tmp/predefined-filters.txt",
         "2ecf84f9f0603d4bb949aa20b9ca4a04",
         "363a79ee038c075d5b5313c41cdfd8a8");

      --  ----- variable substitution with text filters -----------------------
      Settings.Abort_On_Error              := False;
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Filter_Alert      := Error;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/predefined-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/predefined-filters.toml",
         Filters    => null,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>   1,
         Files_Overwritten    =>   1,
         Variables_Defined    =>  12,
         Variables_Found      =>  59,
         Variables_Replaced   =>  59,
         Filters_Found        => 111,
         Filters_Applied      => 110,
         Replacement_Errors   =>   1,
         others               =>   0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);

      Check_MD5_Digest
        ("suite/tmp/predefined-filters.txt",
         "0d929992739b5973fec26faffa591dd7",
         "e159425237dbb9f936b7e000c480780f");
   end Test_Predefined_Filters;

   -------------------------
   -- Test_Custom_Filters --
   -------------------------

   procedure Test_Custom_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      --!pp off
      Filters  : aliased Mold.Filters_Array := [
        0 => Replace_By_Slash'Access,
        2 => Replace_By_Slash'Access,
        others => null
      ];
      --!pp on
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- variable substitution with custom text filters ----------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/custom-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/custom-filters.toml",
         Filters    => Filters'Unchecked_Access,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>  1,
         Variables_Defined    =>  2,
         Variables_Found      => 12,
         Variables_Replaced   => 12,
         Filters_Found        => 14,
         Filters_Applied      => 14,
         Replacement_Warnings =>  0,
         others               =>  0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);

      Check_MD5_Digest
        ("suite/tmp/custom-filters.txt", "2f9e9715d50353c6e973e4b58b172e23",
         "8a79d3f0fd546132f48e745f1afca4c5");

      --  ----- undefined custom text filter ----------------------------------
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Filter_Alert      := Warning;
      --!pp off
      Filters (2) := null;
      Errors := Apply (
         Source     => "suite/mold/custom-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/custom-filters.toml",
         Filters    => Filters'Unchecked_Access,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>  1,
         Files_Overwritten    =>  1,
         Variables_Defined    =>  2,
         Variables_Found      => 12,
         Variables_Replaced   => 12,
         Filters_Found        => 14,
         Filters_Applied      => 13,
         Replacement_Warnings =>  1,
         Replacement_Errors   =>  0,
         others               =>  0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);

      Check_MD5_Digest
        ("suite/tmp/custom-filters.txt", "0d94f1c07a636326a492f4a62d2d9603",
         "8cf044082091ac4223a9321f41191843");

   end Test_Custom_Filters;

   --------------------------
   -- Test_Invalid_Filters --
   --------------------------

   procedure Test_Invalid_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Settings : Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      --  ----- variable substitution with text filters: errors as warnings ---
      Settings.Undefined_Filter_Alert := Warning;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/invalid-filters.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/custom-filters.toml",
         Filters    => null,
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>  1,
         Variables_Defined    =>  2,
         Variables_Found      => 19,
         Variables_Replaced   => 19,
         Filters_Found        => 19,
         Filters_Applied      =>  0,
         Replacement_Warnings => 19,
         others               =>  0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);
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

end Filters_Tests;
