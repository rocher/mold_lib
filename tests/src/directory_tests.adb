-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with GNAT.Source_Info;

with Mold_Lib; use Mold_Lib;
with Support;  use Support;

package body Directory_Tests is

   package Dir renames Ada.Directories;

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
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- all variables replaced ----------------------------------------
      Settings.Replacement_In_Filenames    := False; --  changed in def file
      Settings.Delete_Source_Files         := True;  --  changed in def file
      Settings.Overwrite_Destination_Files := True;
      Settings.Enable_Defined_Settings     := True;
      Settings.On_Undefined                := Warning; --  changed in def file
      --!pp off
      Success := Apply (
         Source    => "suite/dir",
         Settings  => Settings'Unchecked_Access,
         Toml_File => "suite/dir/mold.toml",
         Results   => Results'Unchecked_Access,
         Log_Level => Log.Level
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
         Files_Processed      =>    7,
         Files_Renamed        =>    2,
         Files_Overwritten    =>    0,
         Variables_Defined    =>    3 + 26 + 100,
         Variables_Found      =>  100 +  4 +   9 + 2118 + 1950 + 1736,
         Variables_Undefined  =>    0 +  4 +   9 +    0 +    0 +    0,
         Variables_Replaced   =>  100 +  0 +   0 + 2118 + 1950 + 1736,
         Variables_Ignored    =>    0 +  4 +   9 +    0 +    0 +    0,
         Variables_Emptied    =>    0 +  0 +   0 +    0 +    0 +    0,
         Warnings             =>    0 +  0 +   0 +    0 +    0 +    0,
         others               =>    0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/dir/complex/foo-bar.txt", "fb84d565c9a834dd25c8c3f670c2e46a",
         "25159cb1d5e189c7d7790554edc57256");
      Check_MD5_Digest
        ("suite/dir/complex/foo.txt", "4c179dd0c4cc0c668539a25435286258",
         "6cab9f28a762df56e553fa39883988c0");
      Check_MD5_Digest
        ("suite/dir/complex/lorem-ipsum.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
      Check_MD5_Digest
        ("suite/dir/complex/no-vars.txt", "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");
      Check_MD5_Digest
        ("suite/dir/complex/sub-1/commodo-dolor-elementum.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
      Check_MD5_Digest
        ("suite/dir/complex/sub-2/arcu-bibendum-commodo.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
      Check_MD5_Digest
        ("suite/dir/simple/lorem-ipsum-pars.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");

      --  ----- remove source files -------------------------------------------
      Settings.Replacement_In_Filenames    := False;
      Settings.Delete_Source_Files         := True;
      Settings.Overwrite_Destination_Files := True;
      Settings.On_Undefined                := Warning;

      Dir.Copy_File
        ("suite/mold/foo.txt.mold", "suite/dir-delete/foo.txt.mold");
      Dir.Copy_File
        ("suite/mold/foo-bar.txt.mold", "suite/dir-delete/foo-bar.txt.mold");

      --!pp off
      Success := Apply (
         Source     => "suite/dir-delete",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/toml/foo-bar.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
      );

      Expected := [
         Files_Processed    =>  2,
         Files_Renamed      =>  0,
         Files_Deleted      =>  2,
         Variables_Defined  =>  2,
         Variables_Found    => 13,
         Variables_Replaced => 13,
         others => 0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);
   end Test_In_Place;

   ----------------------
   -- Test_Destination --
   ----------------------

   procedure Test_Destination (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Success  : Boolean;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
      Settings : aliased Settings_Type := Global_Settings.all;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- all variables replaced ----------------------------------------
      Settings.Replacement_In_Filenames    := False; --  changed in def file
      Settings.Delete_Source_Files         := True;  --  changed in def file
      Settings.Overwrite_Destination_Files := True;
      Settings.Enable_Defined_Settings     := True;
      Settings.On_Undefined                := Warning; --  changed in def file
      --!pp off
      Success := Apply (
         Source     => "suite/dir",
         Output_Dir => "suite/tmp/dir",
         Settings   => Settings'Unchecked_Access,
         Toml_File  => "suite/dir/mold.toml",
         Results    => Results'Unchecked_Access,
         Log_Level  => Log.Level
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
         Files_Processed     =>   7,
         Files_Renamed       =>   2,
         Files_Overwritten   =>   0,
         Variables_Defined   =>   3 + 26 + 100,
         Variables_Found     => 100 +  4 +   9 + 2118 + 1950 + 1736,
         Variables_Undefined =>   0 +  4 +   9 +    0 +    0 +    0,
         Variables_Replaced  => 100 +  0 +   0 + 2118 + 1950 + 1736,
         Variables_Ignored   =>   0 +  4 +   9 +    0 +    0 +    0,
         Variables_Emptied   =>   0 +  0 +   0 +    0 +    0 +    0,
         Warnings            =>   0 +  0 +   0 +    0 +    0 +    0,
         others              =>   0
      ];
      --!pp on
      Check_Results
        (Success, True, Results'Unchecked_Access, Expected'Unchecked_Access);

      Check_MD5_Digest
        ("suite/tmp/dir/complex/foo-bar.txt",
         "fb84d565c9a834dd25c8c3f670c2e46a",
         "25159cb1d5e189c7d7790554edc57256");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/foo.txt", "4c179dd0c4cc0c668539a25435286258",
         "6cab9f28a762df56e553fa39883988c0");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/lorem-ipsum.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/no-vars.txt",
         "7ef8e151c0fde9d5fef738709a321300",
         "c81d1f24d9f8018b1760478e1ffe8f98");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/sub-1/commodo-dolor-elementum.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
      Check_MD5_Digest
        ("suite/tmp/dir/complex/sub-2/arcu-bibendum-commodo.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
      Check_MD5_Digest
        ("suite/tmp/dir/simple/lorem-ipsum-pars.txt",
         "ff416bfec859c59a3834c46d60250e25",
         "8880f5a8180491db9710d884c81f4117");
   end Test_Destination;

end Directory_Tests;
