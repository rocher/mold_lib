-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold;

package Results is

   procedure Inc (Results : Mold.Results_Access; Field : Mold.Field_Type);
   --  Increment results' field.

end Results;
