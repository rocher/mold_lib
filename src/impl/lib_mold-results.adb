-------------------------------------------------------------------------------
--
--  Lib_Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Lib_Mold.Results is

   use all type Mold.Results_Access;

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

end Lib_Mold.Results;
