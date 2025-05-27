-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Log_Wrapper;
with Mold_Lib; use Mold_Lib;
with Support;  use Support;

package body Variables_Tests is

   ----------
   -- Name --
   ----------

   overriding
   function Name (T : Variables_Test_Case) return Test_String
   is (Format ("Variables Tests  "));

   --------------------
   -- Register_Tests --
   --------------------

   overriding
   procedure Register_Tests (T : in out Variables_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Variables_Definition'Access, "Variables Definitions");
      Register_Routine (T, Test_No_Substitution'Access, "No Substitution");
      Register_Routine
        (T, Test_Basic_Substitution'Access, "Basic Substitutions");
      Register_Routine
        (T, Test_Modal_Substitution'Access, "Modal Substitution");
      Register_Routine (T, Test_Multiline'Access, "Multiline");
      Register_Routine (T, Test_Show_Variables'Access, "Show Variables");
      Register_Routine
        (T, Test_Predefined_Variables'Access, "Predefined Variables");
      Register_Routine (T, Test_Date_Formats'Access, "Date Formats");
      Register_Routine
        (T, Test_Invalid_Date_Formats'Access, "Invalid Date Formats");
   end Register_Tests;

   procedure Test_Variables_Definition (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin
      --  ----- variables inside variables ------------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/vars-def-1.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/vars-def-1.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    => 1,
         Variables_Defined  => 4,
         Variables_Found    => 2,
         Variables_Replaced => 2,
         others             => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/vars-def-1.txt",
         "3a1f86f00d7f3c412130c9f8b8329c74",
         "c403d6318220fc26784e41e6461855e8");
   end Test_Variables_Definition;

   --------------------------
   -- Test_No_Substitution --
   --------------------------

   procedure Test_No_Substitution (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  ----- no variables in the source file -------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/no-vars.txt",
         "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");

      --  ----- empty definitions file ----------------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Variables_Ignored   => 9,
         Variables_Found     => 9,
         Variables_Undefined => 9,
         Warnings            => 0,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo.txt",
         "4c179dd0c4cc0c668539a25435286258",
         "6cab9f28a762df56e553fa39883988c0");

      --  ----- no variable can be replaced -----------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/foo.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Files_Overwritten   => 1,
         Variables_Defined   => 1,
         Variables_Found     => 9,
         Variables_Undefined => 9,
         Variables_Ignored   => 9,
         Warnings            => 0,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo.txt",
         "4c179dd0c4cc0c668539a25435286258",
         "6cab9f28a762df56e553fa39883988c0");
   end Test_No_Substitution;

   -----------------------------
   -- Test_Basic_Substitution --
   -----------------------------

   procedure Test_Basic_Substitution (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  ----- variable replaced ---------------------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo.txt",
         "3d22c1e66750c3e7925e643cfbe9e327",
         "2858d2c557f2cecc74abff989db01c99");

      --  ----- four variables, two are replaced ------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/foo-bar.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/foo.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Variables_Defined   => 1,
         Variables_Found     => 4,
         Variables_Undefined => 2,
         Variables_Replaced  => 2,
         Variables_Ignored   => 2,
         Warnings            => 0,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo-bar.txt",
         "9fe90f7706a6c0de1155e8e340fafed7",
         "a4123b8c2e3323543173f902e2605f61");

      --  ----- all variables replaced ----------------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/foo-bar.txt",
         "5b6c9393c2233d09b1517bc8c3ca9de1",
         "0ae8639e4d2086703c4f42c300cd0c7b");
   end Test_Basic_Substitution;

   -----------------------------
   -- Test_Modal_Substitution --
   -----------------------------

   procedure Test_Modal_Substitution (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin

      --  ----- all variables replaced ----------------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");

      --  ----- no optional variables defined ---------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "fee4ce163f4e85103e42ab27a49ee381",
         "24ac225ea04e94b461dd56198c9e5561");

      --  ----- some optional variables defined -------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "1ed55361c952f1e572a156c07a3c2f3d",
         "0faaaac0483521b52b19b0832c45855c");

      --  ----- undefined variables ignored and no warning --------------------
      Settings.On_Undefined := Mold.Ignore;
      --!pp off
      Success := Apply (
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
         Warnings            =>    0,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "239eacc9eb868d2d3559a8ee4b903bb1",
         "0b57373fbc9240bf183adfd5eb3fd82b");

      --  ----- undefined variables ignored, warning issued -------------------
      Settings.On_Undefined := Mold.Ignore;
      --!pp off
      Success := Apply (
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
         Warnings            =>    0,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "239eacc9eb868d2d3559a8ee4b903bb1",
         "0b57373fbc9240bf183adfd5eb3fd82b");

      --  ----- undefined variables emptied and no warning --------------------
      Settings.On_Undefined := Mold.Warning;
      --!pp off
      Success := Apply (
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
         Warnings            =>  294,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "a497437f9f4ebc6b42ec0f9aa33dba3d",
         "171564ce81dfde5ca643e2227e8524b7");

      --  ----- undefined variables emptied, warning issued -------------------
      Settings.On_Undefined := Mold.Warning;
      --!pp off
      Success := Apply (
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
         Warnings            =>  294,
         others              =>    0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "a497437f9f4ebc6b42ec0f9aa33dba3d",
         "171564ce81dfde5ca643e2227e8524b7");

      --  ----- undefined mandatory variable ----------------------------------
      Settings.On_Undefined := Mold.Ignore;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/lorem-ipsum.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/lorem-ipsum_mix.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     =>  1,
         Files_Overwritten   =>  1,
         Variables_Defined   => 18,
         Variables_Found     =>  5,
         Variables_Undefined =>  2,
         Variables_Replaced  =>  3,
         Variables_Ignored   =>  2,
         Variables_Emptied   =>  0,
         Warnings            =>  0,
         others              =>  0
      ];
      --!pp on
      Check_Results
        (Success, False, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum.txt",
         "4e0158f382f6db7d8229fd795d51ed3c",
         "6541a8bf51611ddc51f123a8da124ea3");

   end Test_Modal_Substitution;

   --------------------
   -- Test_Multiline --
   --------------------

   procedure Test_Multiline (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  ----- multiline paragraphs ------------------------------------------
      --!pp off
      Success := Apply (
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
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/multiline.txt",
         "cfafd88cdde135c6e27e9917e5a74504",
         "ff09390de79ffd52e39d82c490d336ad");
   end Test_Multiline;

   -------------------------
   -- Test_Show_Variables --
   -------------------------

   procedure Test_Show_Variables (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin

      --  ----- show variables ------------------------------------------------
      --!pp off
      Success := Show_Variables (
         Toml_File => "suite/toml/filters+vars.toml",
         Settings  => Global_Settings,
         Results    => Results'Unchecked_Access,
         Log_Level => Log.Level
      );
      Expected := [
         Variables_Defined => 12,
         others            =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);
   end Test_Show_Variables;

   -------------------------------
   -- Test_Predefined_Variables --
   -------------------------------

   procedure Test_Predefined_Variables (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin

      --  ----- valid predefined variables ------------------------------------
      --!pp off
      Success := Apply (
         Source     => "suite/mold/predefined-vars.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Global_Settings,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>  1,
         Variables_Defined  =>  0,
         Variables_Found    => 22,
         Variables_Replaced => 22,
         others             =>  0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  Do not check the MD5 digest, as it is not deterministic (can vary
      --  depending on the system arch and distro).

      --  ----- invalid predefined variables ----------------------------------
      --!pp off
      Settings.On_Undefined := Mold.Ignore;
      Success := Apply (
         Source     => "suite/mold/invalid-predefined-vars.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Variables_Defined   => 0,
         Variables_Found     => 4,
         Variables_Undefined => 4,
         Variables_Ignored   => 4,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/invalid-predefined-vars.txt",
         "1d3ceb2c6a5600d3f1c71be014ce2b2e",
         "202ab8a256c8e8beb8ee037e80ea8dc1");

      --!pp off
      Settings.On_Undefined := Mold.Warning;
      Success := Apply (
         Source     => "suite/mold/invalid-predefined-vars.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     => 1,
         Files_Overwritten   => 1,
         Variables_Defined   => 0,
         Variables_Found     => 4,
         Variables_Undefined => 4,
         Variables_Ignored   => 0,
         Variables_Emptied   => 4,
         Warnings            => 4,
         others              => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/invalid-predefined-vars.txt",
         "bb4782dcbf7698092160ede9c20413d6",
         "652c9ea5589800a43a13bbf417910b88");
      Log_Wrapper.Log_Debug (Results'Image);
   end Test_Predefined_Variables;

   -----------------------
   -- Test_Date_Formats --
   -----------------------

   procedure Test_Date_Formats (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Settings : Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      --!pp off
      Success := Apply (
         Source     => "suite/mold/date-formats.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed    =>   1,
         Variables_Defined  =>   0,
         Variables_Found    =>  12,
         Variables_Replaced =>  12,
         others             =>  0
      ];
      --!pp on

      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);
   end Test_Date_Formats;

   -------------------------------
   -- Test_Invalid_Date_Formats --
   -------------------------------

   procedure Test_Invalid_Date_Formats (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Settings : Mold.Settings_Type := Global_Settings.all;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      Settings.On_Undefined := Ignore;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/invalid-date-formats.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed     =>  1,
         Variables_Defined   =>  0,
         Variables_Found     => 11,
         Variables_Undefined => 11,
         Variables_Replaced  =>  0,
         Variables_Ignored   => 11,
         Variables_Emptied   =>  0,
         Warnings            =>  0,
         others              =>  0
      ];
      --!pp on

      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  Apply same test with 'Warning' handling

      Settings.On_Undefined := Warning;
      Settings.Overwrite_Destination_Files := True;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/invalid-date-formats.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed            =>  1,
         Mold_Lib.Files_Overwritten =>  1,
         Variables_Defined          =>  0,
         Variables_Found            => 11,
         Variables_Undefined        => 11,
         Variables_Replaced         =>  0,
         Variables_Ignored          =>  0,
         Variables_Emptied          => 11,
         Warnings                   => 11,
         others                     =>  0
      ];
      --!pp on

      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      --  Apply same test with 'Error' handling

      Settings.On_Undefined := Error;
      Settings.Overwrite_Destination_Files := True;
      --!pp off
      Success := Apply (
         Source     => "suite/mold/invalid-date-formats.txt.mold",
         Output_Dir => "suite/tmp/",
         Settings   => Settings'Unrestricted_Access,
         Toml_File  => "suite/toml/empty.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );
      Expected := [
         Files_Processed            =>  1,
         Mold_Lib.Files_Overwritten =>  1,
         Variables_Defined          =>  0,
         Variables_Found            =>  1,
         Variables_Undefined        =>  1,
         Variables_Replaced         =>  0,
         Variables_Ignored          =>  0,
         Variables_Emptied          =>  0,
         Warnings                   =>  0,
         others                     =>  0
      ];
      --!pp on

      Check_Results
        (Success, False, Results'Unchecked_Access, Expected'Unchecked_Access);
   end Test_Invalid_Date_Formats;

end Variables_Tests;
