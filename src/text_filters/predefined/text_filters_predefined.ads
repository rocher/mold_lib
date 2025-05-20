-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Text_Filters_Data; use Text_Filters_Data;

package Text_Filters_Predefined is

   --!pp off
   function Trim_Left     (S : UString) return UString;
   function Trim_Right    (S : UString) return UString;
   function Trim_Both     (S : UString) return UString;
   function Trim_Squash   (S : UString) return UString;
   function Trim_All      (S : UString) return UString;
   function Remove_Blanks (S : UString) return UString;

   function Replace_All   (S : UString; Src, Dst : Character) return UString;
   function Replace_First (S : UString; Src, Dst : Character) return UString;
   function Replace_Last  (S : UString; Src, Dst : Character) return UString;
   function Sequence      (S : UString; Dst      : Character) return UString;
   function Delete_All    (S : UString; Src      : Character) return UString;

   function Case_Lowercase (S : UString) return UString;
   function Case_Capitals  (S : UString) return UString;
   function Case_Uppercase (S : UString) return UString;

   function Padding  (Value  : UString;
                      Dir    : Direction;
                      Char   : Character;
                      Length : Natural) return UString;

   function Truncate (S : UString; Length : Natural) return UString;

   function Style_Flat_Case        (S : UString) return UString;
   function Style_Lower_Camel_Case (S : UString) return UString;
   function Style_Upper_Camel_Case (S : UString) return UString;
   function Style_Uppercase        (S : UString) return UString;
   function Style_Snake_Case       (S : UString) return UString;
   function Style_Camel_Snake_Case (S : UString) return UString;
   function Style_Title_Case       (S : UString) return UString;
   function Style_All_Caps         (S : UString) return UString;
   function Style_Dash_Case        (S : UString) return UString;
   function Style_Train_Case       (S : UString) return UString;
   function Style_Train_Uppercase  (S : UString) return UString;
   --!pp on

end Text_Filters_Predefined;
