-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold is

   type Undef_Var_Action is (Ignore, Empty);
   type Undef_Var_Alert is (None, Warning);

   type Settings_Type is record
      Rename_Source    : aliased Boolean;
      Delete_Source    : aliased Boolean;
      Overwrite        : aliased Boolean;
      Defined_Settings : aliased Boolean;
      Action           : aliased Undef_Var_Action;
      Alert            : aliased Undef_Var_Alert;
      Abort_On_Error   : aliased Boolean;
   end record;
   type Settings_Access is access all Settings_Type;

end Mold;
