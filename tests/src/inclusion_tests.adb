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

package body Inclusion_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Inclusion_Test_Case) return Test_String is
     (Format ("Inclusion Tests  "));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Inclusion_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Recursive_Inclusion'Access,
         "Prevent Recursive  Inclusion of Templates");
      Register_Routine (T, Test_Inclusion'Access, "Inclusion of Templates");
   end Register_Tests;

   ------------------------------
   -- Test_Recursive_Inclusion --
   ------------------------------

   procedure Test_Recursive_Inclusion (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- inclusion of recursive templates ------------------------------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/recursion.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/foo.toml",
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Level
      );
      Expected := [
         Files_Processed      => 1,
         Files_Renamed        => 0,
         Files_Overwritten    => 0,
         Variables_Defined    => 1,
         Variables_Found      => 0,
         Variables_Undefined  => 0,
         Variables_Replaced   => 0,
         Variables_Ignored    => 0,
         Variables_Emptied    => 0,
         Replacement_Warnings => 0,
         Replacement_Errors   => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);
   end Test_Recursive_Inclusion;

   --------------------
   -- Test_Inclusion --
   --------------------

   procedure Test_Inclusion (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- inclusion of 100 templates ------------------------------------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/lorem-ipsum-includes-01.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum.toml",
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Level
      );
      Expected := [
         Files_Processed      =>    1,
         Files_Renamed        =>    0,
         Files_Overwritten    =>    0,
         Variables_Defined    =>   26,
         Variables_Found      => 2118,
         Variables_Undefined  =>    0,
         Variables_Replaced   => 2118,
         Variables_Ignored    =>    0,
         Variables_Emptied    =>    0,
         Replacement_Warnings =>    0,
         Replacement_Errors   =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum-includes-01.txt",
         "ff416bfec859c59a3834c46d60250e25");

      --  ----- inclusion of 100 templates ------------------------------------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/lorem-ipsum-includes-02.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum.toml",
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Level
      );
      Expected := [
         Files_Processed      =>    1,
         Files_Renamed        =>    0,
         Files_Overwritten    =>    0,
         Variables_Defined    =>   26,
         Variables_Found      => 2118,
         Variables_Undefined  =>    0,
         Variables_Replaced   => 2118,
         Variables_Ignored    =>    0,
         Variables_Emptied    =>    0,
         Replacement_Warnings =>    0,
         Replacement_Errors   =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum-includes-02.txt",
         "ff416bfec859c59a3834c46d60250e25");

      --  ----- inclusion of 100 templates ------------------------------------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/lorem-ipsum-includes-03.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/toml/lorem-ipsum.toml",
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Level
      );
      Expected := [
         Files_Processed      =>    1,
         Files_Renamed        =>    0,
         Files_Overwritten    =>    0,
         Variables_Defined    =>   26,
         Variables_Found      => 2118,
         Variables_Undefined  =>    0,
         Variables_Replaced   => 2118,
         Variables_Ignored    =>    0,
         Variables_Emptied    =>    0,
         Replacement_Warnings =>    0,
         Replacement_Errors   =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/lorem-ipsum-includes-03.txt",
         "ff416bfec859c59a3834c46d60250e25");

   end Test_Inclusion;

end Inclusion_Tests;
