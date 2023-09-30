-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Text_Filters_Data is

   subtype UString is Unbounded_String;

   type Replace is (r_all, first, last);
   type Direction is (left, right);

   --!pp off
   type Text_Filter_Type is (
      filter_none,
      filter_error,
      filter_custom,

      filter_trim_left,
      filter_trim_right,
      filter_trim_both,
      filter_trim_squash,
      filter_trim_all,
      filter_remove_blanks,

      filter_replace_all,
      filter_replace_first,
      filter_replace_last,
      filter_sequence,
      filter_delete_all,

      filter_padding,
      filter_truncate,

      filter_case_lowercase,
      filter_case_capitals,
      filter_case_uppercase,

      filter_style_flat_case,
      filter_style_lower_camel_case,
      filter_style_upper_camel_case,
      filter_style_uppercase,
      filter_style_snake_case,
      filter_style_camel_snake_case,
      filter_style_title_case,
      filter_style_all_caps,
      filter_style_dash_case,
      filter_style_train_case,
      filter_style_train_uppercase
   );
   --!pp on

   type Text_Filter_Parsed is record
      Kind   : Text_Filter_Type := filter_none;
      Src    : Character        := ' ';
      Dst    : Character        := ' ';
      Dir    : Direction        := left;
      Number : Natural          := 0;
      Error  : UString          := Null_Unbounded_String;
   end record;

end Text_Filters_Data;
