-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

-----------
-- Apply --
-----------

separate (Text_Filters)
function Apply (Filter : Text_Filter_Parsed; S : UString) return UString is
begin
   case Filter.Kind is
      when filter_custom =>
         return
           To_Unbounded_String
             (Custom_Filters.all (Filter.Number) (To_String (S)));

      --!pp off
      when filter_trim_left     => return Trim_Left (S);
      when filter_trim_right    => return Trim_Right (S);
      when filter_trim_both     => return Trim_Both (S);
      when filter_trim_squash   => return Trim_Squash (S);
      when filter_trim_all      => return Trim_All (S);
      when filter_remove_blanks => return Remove_Blanks (S);
      --!pp on

      when filter_replace_all =>
         return Replace_All (S, Filter.Src, Filter.Dst);
      when filter_replace_first =>
         return Replace_First (S, Filter.Src, Filter.Dst);
      when filter_replace_last =>
         return Replace_Last (S, Filter.Src, Filter.Dst);
      when filter_sequence =>
         return Sequence (S, Filter.Dst);
      when filter_delete_all =>
         return Delete_All (S, Filter.Src);

      when filter_padding =>
         return Padding (S, Filter.Dir, Filter.Src, Filter.Number);
      when filter_truncate =>
         return Truncate (S, Filter.Number);

      --!pp off
      when filter_case_lowercase => return Case_Lowercase (S);
      when filter_case_capitals  => return Case_Capitals (S);
      when filter_case_uppercase => return Case_Uppercase (S);

      when filter_style_flat_case        => return Style_Flat_Case (S);
      when filter_style_lower_camel_case => return Style_Lower_Camel_Case (S);
      when filter_style_upper_camel_case => return Style_Upper_Camel_Case (S);
      when filter_style_uppercase        => return Style_Uppercase (S);
      when filter_style_snake_case       => return Style_Snake_Case (S);
      when filter_style_camel_snake_case => return Style_Camel_Snake_Case (S);
      when filter_style_title_case       => return Style_Title_Case (S);
      when filter_style_all_caps         => return Style_All_Caps (S);
      when filter_style_dash_case        => return Style_Dash_Case (S);
      when filter_style_train_case       => return Style_Train_Case (S);
      when filter_style_train_uppercase  => return Style_Train_Uppercase (S);
      --!pp on

         pragma Annotate
           (Xcov, Exempt_On, "Precondition guarantees no other values");
      when others =>
         return Null_Unbounded_String;
         pragma Annotate (Xcov, Exempt_On);

   end case;
end Apply;
