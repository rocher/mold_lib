-------------------------------------------------------------------------------
--
--  Lib_Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Results is

   use all type Mold.Results_Access;

   ---------
   -- Inc --
   ---------

   procedure Inc
     (Results : Mold.Results_Access; Field : Mold.Results_Field_Type)
   is
   begin
      if Results /= null then
         Results.all (Field) := @ + 1;
      end if;
   end Inc;

end Results;
