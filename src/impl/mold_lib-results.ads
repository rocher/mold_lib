-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Results is

   package Mold renames Mold_Lib;

   procedure Inc (Results : Mold.Results_Access; Field : Mold.Results_Fields);
   --  If Results is not null, increment the given Field.

end Mold_Lib.Results;
