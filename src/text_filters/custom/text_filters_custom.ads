-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Text_Filters_Custom is

   type Text_Filter is access function (S : String) return String;
   --  Function that transform a String into another String.

   type Filters_Array is array (0 .. 9) of Text_Filter;
   --  Array of up to 10 custom-defined text filters.

   type Filters_Access is access all Filters_Array;
   --  Pointer to an array of pointers to functions.

end Text_Filters_Custom;
