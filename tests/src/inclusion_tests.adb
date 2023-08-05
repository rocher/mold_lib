-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold;    use Mold;
with Support; use Support;

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
      Register_Routine (T, Test_Inclusion'Access, "Inclusion of Templates");
   end Register_Tests;

   --------------------
   -- Test_Inclusion --
   --------------------

   procedure Test_Inclusion (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin
      --  ----- inclusion of 100 templates ------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/lorem-ipsum-includes-01.txt.mold",
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
        ("suite/tmp/lorem-ipsum-includes-01.txt",
         "ff416bfec859c59a3834c46d60250e25");

      --  ----- inclusion of 100 templates ------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/lorem-ipsum-includes-02.txt.mold",
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
        ("suite/tmp/lorem-ipsum-includes-02.txt",
         "ff416bfec859c59a3834c46d60250e25");

      --  ----- inclusion of 100 templates ------------------------------------
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/mold/lorem-ipsum-includes-03.txt.mold",
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
        ("suite/tmp/lorem-ipsum-includes-03.txt",
         "ff416bfec859c59a3834c46d60250e25");

   end Test_Inclusion;

end Inclusion_Tests;
