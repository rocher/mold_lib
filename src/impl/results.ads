-------------------------------------------------------------------------------
--
--  Lib_Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Lib_Mold;

package Results is

   package Mold renames Lib_Mold;

   procedure Inc (Results : Mold.Results_Access; Field : Mold.Results_Fields);
   --  Increment results' field.

   --  *TODO - try to integrate this function in another file

end Results;
