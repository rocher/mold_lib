-------------------------------------------------------------------------------
--
--  Lib_Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Lib_Mold.Results is

   package Mold renames Lib_Mold;

   procedure Inc (Results : Mold.Results_Access; Field : Mold.Results_Fields);
   --  If Results is not null, increment the given Field.

end Lib_Mold.Results;
