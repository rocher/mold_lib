-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

with AUnit.Assertions; use AUnit.Assertions;

with Simple_Logging;

with Mold;    use Mold;
with Support; use Support;

package body Variables_Tests is

   ----------
   -- Name --
   ----------

   overriding function Name (T : Variables_Test_Case) return Test_String is
     (Format ("Variables Tests"));

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Variables_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_No_Substitution'Access, "No Substitutions Expected");
   end Register_Tests;

   procedure Test_No_Substitution (T : in out Test_Case'Class) is
      Errors   : Natural;
      Results  : aliased Mold.Results_Type;
      Expected : aliased Mold.Results_Type;
   begin
      Errors   :=
        Mold.Apply
          (Source => "suite/mold/no_vars.txt.mold", Output_Dir => "suite/tmp/",
           Settings => Global_Settings, Definitions => "suite/toml/empty.toml",
           Results  => Results'Unchecked_Access);
      Expected :=
        [Files    => 1, Renamed => 0, Overwritten => 0, Definitions => 0,
        Variables => 0, Undefined => 0, Substituted => 0, Ignored => 0,
        Emptied   => 0, Warnings => 0, Mold.Errors => 0];

      Simple_Logging.Detail (Pretty_Print (Errors, Results'Unchecked_Access));
      Assert
        (Errors = 0 or else Results (Mold.Errors) = 0,
         "Incorrect error reported in test 01");
      Check_Results (Results'Unchecked_Access, Expected'Unchecked_Access);

      Errors   :=
        Mold.Apply
          (Source   => "suite/mold/foo.txt.mold", Output_Dir => "suite/tmp/",
           Settings => Global_Settings, Definitions => "suite/toml/empty.toml",
           Results  => Results'Unchecked_Access);
      Expected :=
        [Files    => 1, Renamed => 0, Overwritten => 0, Definitions => 0,
        Variables => 9, Undefined => 9, Substituted => 0, Ignored => 9,
        Emptied   => 0, Warnings => 9, Mold.Errors => 0];

      Simple_Logging.Detail (Pretty_Print (Errors, Results'Unchecked_Access));
      Assert
        (Errors = 0 or else Results (Mold.Errors) = 0,
         "Incorrect error reported in test 01");
      Check_Results (Results'Unchecked_Access, Expected'Unchecked_Access);

   end Test_No_Substitution;

end Variables_Tests;
