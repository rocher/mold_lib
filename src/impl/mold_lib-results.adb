-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Mold_Lib.Results is

   ---------
   -- Inc --
   ---------

   procedure Inc (Results : Mold.Results_Access; Field : Mold.Results_Fields)
   is
   begin
      if Results /= null then
         Results.all (Field) := @ + 1;
      end if;
   end Inc;

end Mold_Lib.Results;
