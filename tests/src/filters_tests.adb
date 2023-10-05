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

with Mold_Lib_Tests_Config; use Mold_Lib_Tests_Config;

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
      --  Register_Routine (T, Test_Custom_Filters'Access, "Custom Filters");
   end Register_Tests;

   -----------------------------
   -- Test_Predefined_Filters --
   -----------------------------

   procedure Test_Predefined_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
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

      --  ----- variable substitution with text filters: abort on error -------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/predefined-filters.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Toml_File   => "suite/toml/predefined-filters.toml",
         Filters     => Filters'Unchecked_Access,
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Debug
      );
      Expected := [
         Files_Processed      =>  1,
         Variables_Defined    => 12,
         Variables_Found      => 18,
         Variables_Replaced   => 18,
         Filters_Found        => 67,
         Filters_Applied      => 66,
         Replacement_Warnings =>  1,
         others               =>  0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 1);
      if Alire_Host_OS in "windows" then
         Check_MD5_Digest
           ("suite/tmp/predefined-filters.txt",
            "746433a25741e050806b17fc58866ae5");
      else
         Check_MD5_Digest
           ("suite/tmp/predefined-filters.txt",
            "4fe3a5f40d7cd4c591119da81d5df8cb");
      end if;

   end Test_Predefined_Filters;

   -------------------------
   -- Test_Custom_Filters --
   -------------------------

   procedure Test_Custom_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
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

      --  ----- variable substitution with text filters -----------------------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/filters.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Toml_File   => "suite/toml/filters.toml",
         Filters     => Filters'Unchecked_Access,
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Level
      );
      Expected := [
         Files_Processed      =>  1,
         Variables_Defined    => 12,
         Variables_Found      => 62,
         Variables_Replaced   => 62,
         Filters_Found        => 59,
         Filters_Applied      => 58,
         Replacement_Warnings =>  1,
         others               =>  0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);

   end Test_Custom_Filters;

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
