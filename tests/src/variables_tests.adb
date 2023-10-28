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

package body Variables_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Variables_Test_Case) return Test_String is
     (Format ("Variables Tests  "));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Variables_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_No_Substitution'Access, "No Substitution");
      Register_Routine
        (T, Test_Basic_Substitution'Access, "Basic Substitutions");
      Register_Routine
        (T, Test_Modal_Substitution'Access, "Modal Substitution");
      Register_Routine (T, Test_Multiline'Access, "Multiline");
   end Register_Tests;

   --------------------------
   -- Test_No_Substitution --
   --------------------------

   procedure Test_No_Substitution (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- no variables in the source file -------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/no-vars.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed => 1,
         others          => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars.txt", "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- empty definitions file ----------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      => 1,
         Variables_Ignored    => 9,
         Variables_Found      => 9,
         Variables_Undefined  => 9,
         Replacement_Warnings => 9,
         others               => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo.txt", "4c179dd0c4cc0c668539a25435286258",
         "6cab9f28a762df56e553fa39883988c0");

      --  ----- no variable can be replaced -----------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      => 1,
         Files_Overwritten    => 1,
         Variables_Defined    => 1,
         Variables_Found      => 9,
         Variables_Undefined  => 9,
         Variables_Ignored    => 9,
         Replacement_Warnings => 9,
         others               => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo.txt", "4c179dd0c4cc0c668539a25435286258",
         "6cab9f28a762df56e553fa39883988c0");
   end Test_No_Substitution;

   -----------------------------
   -- Test_Basic_Substitution --
   -----------------------------

   procedure Test_Basic_Substitution (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- variable replaced ---------------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/foo.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    => 1,
         Files_Overwritten  => 1,
         Variables_Defined  => 1,
         Variables_Found    => 9,
         Variables_Replaced => 9,
         others             => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo.txt", "3d22c1e66750c3e7925e643cfbe9e327",
         "2858d2c557f2cecc74abff989db01c99");

      --  ----- four variables, two are replaced ------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo-bar.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/foo.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      => 1,
         Variables_Defined    => 1,
         Variables_Found      => 4,
         Variables_Undefined  => 2,
         Variables_Replaced   => 2,
         Variables_Ignored    => 2,
         Replacement_Warnings => 2,
         others               => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo-bar.txt", "9fe90f7706a6c0de1155e8e340fafed7",
         "a4123b8c2e3323543173f902e2605f61");

      --  ----- all variables replaced ----------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/foo-bar.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/foo-bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    => 1,
         Files_Overwritten  => 1,
         Variables_Defined  => 2,
         Variables_Found    => 4,
         Variables_Replaced => 4,
         others             => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo-bar.txt", "5b6c9393c2233d09b1517bc8c3ca9de1",
         "0ae8639e4d2086703c4f42c300cd0c7b");
   end Test_Basic_Substitution;

   -----------------------------
   -- Test_Modal_Substitution --
   -----------------------------

   procedure Test_Modal_Substitution (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- all variables replaced ----------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>    1,
         Variables_Defined  =>   26,
         Variables_Found    => 2118,
         Variables_Replaced => 2118,
         others             =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");

      --  ----- no optional variables defined ---------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_no-opts1.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     =>    1,
         Files_Overwritten   =>    1,
         Variables_Defined   =>   23,
         Variables_Found     => 2118,
         Variables_Undefined =>  291,
         Variables_Replaced  => 1827,
         Variables_Emptied   =>  291,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "fee4ce163f4e85103e42ab27a49ee381",
         "24ac225ea04e94b461dd56198c9e5561");

      --  ----- some optional variables defined -------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_no-opts2.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     =>    1,
         Files_Overwritten   =>    1,
         Variables_Defined   =>   24,
         Variables_Found     => 2118,
         Variables_Undefined =>  176,
         Variables_Replaced  => 1942,
         Variables_Emptied   =>  176,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "1ed55361c952f1e572a156c07a3c2f3d",
         "0faaaac0483521b52b19b0832c45855c");

      --  ----- undefined variables ignored and no warning --------------------
      Settings.Undefined_Action := Mold.Ignore;
      Settings.Undefined_Alert  := Mold.None;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_no-norm1.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     =>    1,
         Files_Overwritten   =>    1,
         Variables_Defined   =>   24,
         Variables_Found     => 2118,
         Variables_Undefined =>  294,
         Variables_Replaced  => 1824,
         Variables_Ignored   =>  294,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "239eacc9eb868d2d3559a8ee4b903bb1",
         "0b57373fbc9240bf183adfd5eb3fd82b");

      --  ----- undefined variables ignored, warning issued -------------------
      Settings.Undefined_Action := Mold.Ignore;
      Settings.Undefined_Alert  := Mold.Warning;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_no-norm1.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>    1,
         Files_Overwritten    =>    1,
         Variables_Defined    =>   24,
         Variables_Found      => 2118,
         Variables_Undefined  =>  294,
         Variables_Replaced   => 1824,
         Variables_Ignored    =>  294,
         Replacement_Warnings =>  294,
         others               =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "239eacc9eb868d2d3559a8ee4b903bb1",
         "0b57373fbc9240bf183adfd5eb3fd82b");

      --  ----- undefined variables emptied and no warning --------------------
      Settings.Undefined_Action := Mold.Empty;
      Settings.Undefined_Alert  := Mold.None;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_no-norm1.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     =>    1,
         Files_Overwritten   =>    1,
         Variables_Defined   =>   24,
         Variables_Found     => 2118,
         Variables_Undefined =>  294,
         Variables_Replaced  => 1824,
         Variables_Emptied   =>  294,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "a497437f9f4ebc6b42ec0f9aa33dba3d",
         "171564ce81dfde5ca643e2227e8524b7");

      --  ----- undefined variables emptied, warning issued -------------------
      Settings.Undefined_Action := Mold.Empty;
      Settings.Undefined_Alert  := Mold.Warning;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_no-norm1.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>    1,
         Files_Overwritten    =>    1,
         Variables_Defined    =>   24,
         Variables_Found      => 2118,
         Variables_Undefined  =>  294,
         Variables_Replaced   => 1824,
         Variables_Emptied    =>  294,
         Replacement_Warnings =>  294,
         others               =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "a497437f9f4ebc6b42ec0f9aa33dba3d",
         "171564ce81dfde5ca643e2227e8524b7");

      --  ----- undefined mandatory variable, no abort on error ---------------
      Settings.Undefined_Action := Mold.Ignore;
      Settings.Undefined_Alert  := Mold.Warning;
      Settings.Abort_On_Error            := False;
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_mix.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed      =>    1,
         Files_Overwritten    =>    1,
         Variables_Defined    =>   18,
         Variables_Found      => 2118,
         Variables_Undefined  =>  591,
         Variables_Replaced   => 1527,
         Variables_Ignored    =>  405,
         Variables_Emptied    =>  186,
         Replacement_Warnings =>  315,
         Replacement_Errors   =>   90,
         others               =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 90);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt", "caa552768a9819fff5eb93f4096189c3",
         "87b82554bef807a9a230edd986473700");

   end Test_Modal_Substitution;

   --------------------
   -- Test_Multiline --
   --------------------

   procedure Test_Multiline (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- multiline paragraphs ------------------------------------------
      --!pp off
      Errors := Apply (
         Source     => "suite/mold/multiline.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/multiline.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    => 1,
         Variables_Defined  => 4,
         Variables_Found    => 4,
         Variables_Replaced => 4,
         others             => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/multiline.txt", "cfafd88cdde135c6e27e9917e5a74504",
         "ff09390de79ffd52e39d82c490d336ad");
   end Test_Multiline;

end Variables_Tests;
