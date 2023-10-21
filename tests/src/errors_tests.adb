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

package body Errors_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Errors_Test_Case) return Test_String is
     (Format ("Errors Tests     "));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Errors_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Variable_Errors'Access, "Errors during variable substitution");
      Register_Routine
        (T, File_Errors'Access, "Errors during file operations");
      Register_Routine
        (T, Directory_Errors'Access, "Errors during directory operations");
      Register_Routine
        (T, Validations_Errors'Access, "Errors during validations operations");
   end Register_Tests;

   ---------------------
   -- Variable_Errors --
   ---------------------

   procedure Variable_Errors (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- undefined variable --------------------------------------------
      Settings.Abort_On_Error              := True;
      Settings.Overwrite_Destination_Files := True;
      Settings.Undefined_Variable_Alert    := Error;
      Results                              := [others => 0];
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Files_Overwritten   => 1,
         Variables_Defined   => 1,
         Variables_Found     => 1,
         Variables_Undefined => 1,
         Variables_Ignored   => 1,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);

      --  ----- invalid mold setting ------------------------------------------
      Settings.Abort_On_Error := False;
      Results                 := [others => 0];
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/invalid-variable-setting.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Variables_Defined => 1,
         others            => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);
   end Variable_Errors;

   -----------------
   -- File_Errors --
   -----------------

   procedure File_Errors (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- non-existent file ---------------------------------------------
      Settings.Abort_On_Error := True;
      Results                 := [others => 0];
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/unknown-file.mold",
         Output_Dir => "suite/tmp",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [ others => 0 ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);

      --  ----- non-existent include file -------------------------------------
      Settings.Abort_On_Error := True;
      Results                 := [others => 0];
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/invalid-include.mold",
         Output_Dir => "suite/tmp",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/foo.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Variables_Defined   => 1,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);

   end File_Errors;

   ----------------------
   -- Directory_Errors --
   ----------------------

   procedure Directory_Errors (T : in out Test_Case'Class) is
   begin
      null;
   end Directory_Errors;

   ------------------------
   -- Validations_Errors --
   ------------------------

   procedure Validations_Errors (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors : Natural;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- invalid source path -------------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/toml/foo.toml",
         Output_Dir => "suite/invalid_dir",
         Toml_File  => "suite/toml/foo.toml"
      );
      --!pp on
      Check_Errors (Errors, 1);

      --!pp off
      Errors := Apply (
         Source     => "invalid:source:file",
         Output_Dir => "suite/invalid_dir",
         Toml_File  => "suite/toml/foo.toml"
      );
      --!pp on
      Check_Errors (Errors, 1);

      --  ----- invalid toml file ---------------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp",
         Toml_File  => "suite/toml/invalid.toml"
      );
      --!pp on
      Check_Errors (Errors, 1);

      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp",
         Toml_File  => "/invalid:path/foo.toml"
      );
      --!pp on
      Check_Errors (Errors, 1);

      --  ----- invalid directory ---------------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "/invalid:dir:name/",
         Toml_File  => "suite/toml/foo.toml"
      );
      --!pp on
      Check_Errors (Errors, 1);

      --  --!pp off
      --  Errors := Apply (
      --     Source     => "suite/mold/foo.txt.mold",
      --     Output_Dir => "/dev/null",
      --     Toml_File  => "suite/toml/foo.toml"
      --  );
      --  --!pp on
      --  Check_Errors (Errors, 1);

   end Validations_Errors;

end Errors_Tests;
