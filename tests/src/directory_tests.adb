-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold;    use Mold;
with Support; use Support;

package body Directory_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Directory_Test_Case) return Test_String is
     (Format ("Directory Tests  "));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Directory_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_In_Place'Access, "In place simple and complex directories");
      Register_Routine
        (T, Test_Destination'Access,
         "Simple and complex directories with different destination");
   end Register_Tests;

   -------------------
   -- Test_In_Place --
   -------------------

   procedure Test_In_Place (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin

      --  ----- all variables replaced ----------------------------------------
      Settings.Rename_Source    := False;      --  changed in def file
      Settings.Delete_Source    := True;       --  changed in def file
      Settings.Overwrite        := True;
      Settings.Defined_Settings := True;
      Settings.Action           := Mold.Empty; --  changed in def file
      Settings.Alert            := Mold.None;  --  changed in def file
      Settings.Abort_On_Error   := True;       --  changed in def file
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/dir",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/dir/mold.toml",
         Results     => Results'Unchecked_Access
      );

      --  FILE NAME                     VARS  RENAMED TO
      --  simple/
      --    lorem-ipsum-pars.txt.mold    100  lorem-ipsum-pars.txt
      --  complex/
      --    foo-bar.txt.mold               4  foo-bar.txt
      --    foo.txt.mold                   9  foo.txt
      --    lorem-ipsum.txt.mold        2118  lorem-ipsum.txt
      --    no-vars.txt.mold               0  no-vars.txt
      --  complex/sub-1/
      --    __c__-__d__-__e__.txt.mold  1950  commodo-dolor-elementum.txt
      --  complex/sub-2/
      --    __a__-__b__-__c__.txt.mold  1736  arcu-bibendum-commodo.txt

      Expected := [
         Files       =>    7,
         Renamed     =>    2,
         Overwritten =>    0,
         Definitions =>    5 + 26 + 100,
         Variables   =>  100 +  4 +   9 + 2118 + 1950 + 1736,
         Undefined   =>    0 +  4 +   9 +    0 +    0 +    0,
         Replaced    =>  100 +  0 +   0 + 2118 + 1950 + 1736,
         Ignored     =>    0 +  4 +   9 +    0 +    0 +    0,
         Emptied     =>    0 +  0 +   0 +    0 +    0 +    0,
         Warnings    =>    0 +  4 +   9 +    0 +    0 +    0,
         Mold.Errors =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/dir/complex/foo-bar.txt", "fb84d565c9a834dd25c8c3f670c2e46a");
      Check_MD5_Digest
        ("suite/dir/complex/foo.txt", "4c179dd0c4cc0c668539a25435286258");
      Check_MD5_Digest
        ("suite/dir/complex/lorem-ipsum.txt",
         "ff416bfec859c59a3834c46d60250e25");
      Check_MD5_Digest
        ("suite/dir/complex/no-vars.txt", "7ef8e151c0fde9d5fef738709a321300");
      Check_MD5_Digest
        ("suite/dir/complex/sub-1/commodo-dolor-elementum.txt",
         "ff416bfec859c59a3834c46d60250e25");
      Check_MD5_Digest
        ("suite/dir/complex/sub-2/arcu-bibendum-commodo.txt",
         "ff416bfec859c59a3834c46d60250e25");
      Check_MD5_Digest
        ("suite/dir/simple/lorem-ipsum-pars.txt",
         "ff416bfec859c59a3834c46d60250e25");

   end Test_In_Place;

   ----------------------
   -- Test_Destination --
   ----------------------

   procedure Test_Destination (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
      Settings : aliased Mold.Settings_Type := Global_Settings.all;
   begin

      --  ----- all variables replaced ----------------------------------------
      Settings.Rename_Source    := False;      --  changed in def file
      Settings.Delete_Source    := True;       --  changed in def file
      Settings.Overwrite        := True;
      Settings.Defined_Settings := True;
      Settings.Action           := Mold.Empty; --  changed in def file
      Settings.Alert            := Mold.None;  --  changed in def file
      Settings.Abort_On_Error   := True;       --  changed in def file
      --!pp off
      Errors := Mold.Apply (
         Source      => "suite/dir",
         Output_Dir  => "suite/tmp/dir",
         Settings    => Settings'Unchecked_Access,
         Definitions => "suite/dir/mold.toml",
         Results     => Results'Unchecked_Access
      );

      --  FILE NAME                     VARS  RENAMED TO
      --  simple/
      --    lorem-ipsum-pars.txt.mold    100  lorem-ipsum-pars.txt
      --  complex/
      --    foo-bar.txt.mold               4  foo-bar.txt
      --    foo.txt.mold                   9  foo.txt
      --    lorem-ipsum.txt.mold        2118  lorem-ipsum.txt
      --    no-vars.txt.mold               0  no-vars.txt
      --  complex/sub-1/
      --    __c__-__d__-__e__.txt.mold  1950  commodo-dolor-elementum.txt
      --  complex/sub-2/
      --    __a__-__b__-__c__.txt.mold  1736  arcu-bibendum-commodo.txt

      Expected := [
         Files       =>    7,
         Renamed     =>    2,
         Overwritten =>    0,
         Definitions =>    5 + 26 + 100,
         Variables   =>  100 +  4 +   9 + 2118 + 1950 + 1736,
         Undefined   =>    0 +  4 +   9 +    0 +    0 +    0,
         Replaced    =>  100 +  0 +   0 + 2118 + 1950 + 1736,
         Ignored     =>    0 +  4 +   9 +    0 +    0 +    0,
         Emptied     =>    0 +  0 +   0 +    0 +    0 +    0,
         Warnings    =>    0 +  4 +   9 +    0 +    0 +    0,
         Mold.Errors =>    0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access);
      Check_MD5_Digest
        ("suite/tmp/dir/complex/foo-bar.txt",
         "fb84d565c9a834dd25c8c3f670c2e46a");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/foo.txt", "4c179dd0c4cc0c668539a25435286258");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/lorem-ipsum.txt",
         "ff416bfec859c59a3834c46d60250e25");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/no-vars.txt",
         "7ef8e151c0fde9d5fef738709a321300");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/sub-1/commodo-dolor-elementum.txt",
         "ff416bfec859c59a3834c46d60250e25");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/sub-2/arcu-bibendum-commodo.txt",
         "ff416bfec859c59a3834c46d60250e25");
      Check_MD5_Digest
        ("suite/tmp/dir/simple/lorem-ipsum-pars.txt",
         "ff416bfec859c59a3834c46d60250e25");

   end Test_Destination;

end Directory_Tests;
