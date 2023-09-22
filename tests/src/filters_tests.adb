-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Source_Info;

--  with AAA.Text_IO;

with Mold_Lib; use Mold_Lib;
with Support;  use Support;

package body Filters_Tests is

   function Number_Pad (S : String) return String;
   function Replace_By_Dash (S : String) return String;
   function To_Title_Case (S : String) return String;

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
        (T, Test_Filters'Access, "Variable substitution with text filters");
   end Register_Tests;

   ------------------
   -- Test_Filters --
   ------------------

   procedure Test_Filters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Errors   : Natural;
      Results  : aliased Results_Type;
      Expected : aliased Results_Type;
   begin
      Log.Debug ("UNIT TEST " & GNAT.Source_Info.Enclosing_Entity);

      --  ----- variable substitution with text filters -----------------------
      --!pp off
      Errors := Apply (
         Source      => "suite/mold/filters.txt.mold",
         Output_Dir  => "suite/tmp/",
         Settings    => Global_Settings,
         Definitions => "suite/toml/filters.toml",
         Filters     => [
            0 => Replace_By_Dash'Access,
            1 => To_Title_Case'Access,
            4 => Number_Pad'Access,
            5 => Number_Pad'Access,
            others => null],
         Results     => Results'Unchecked_Access,
         Log_Level   => Log.Debug
      );
      Expected := [
         Files_Processed      => 1,
         Variables_Defined    => 3,
         Variables_Found      => 9,
         Variables_Replaced   => 9,
         Filters_Found        => 6,
         Filters_Applied      => 5,
         Replacement_Warnings => 1,
         others               => 0
      ];
      --!pp on
      Check_Results
        (Errors, Results'Unchecked_Access, Expected'Unchecked_Access, 0);
   end Test_Filters;

   ----------------
   -- Number_Pad --
   ----------------

   function Number_Pad (S : String) return String is
      J : Natural := 4;
   begin
      return Pad : String (1 .. 4) := "0000" do
         for I in 1 .. S'Length loop
            Pad (J) := S (I);
            J       := J - 1;
         end loop;
         Log.Error ("Pad = " & Pad);
      end return;
   end Number_Pad;

   ---------------------
   -- Replace_By_Dash --
   ---------------------

   function Replace_By_Dash (S : String) return String is
   begin
      return Dash : String (1 .. S'Length) do
         for D of Dash loop
            D := '-';
         end loop;
      end return;
   end Replace_By_Dash;

   -------------------
   -- To_Title_Case --
   -------------------

   function To_Title_Case (S : String) return String is
      Uppercase : Boolean := True;
   begin
      return Title : String := S do
         for T of Title loop
            if T = ' ' then
               T         := '_';
               Uppercase := True;
            else
               if Uppercase then
                  T         := To_Upper (T);
                  Uppercase := False;
               end if;
            end if;
         end loop;
      end return;
   end To_Title_Case;

end Filters_Tests;
