-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Regpat;

with Custom_Text_Filters; use Custom_Text_Filters;

package Text_Filters is

   package IO renames Ada.Text_IO;

   subtype UString is Unbounded_String;

   type Results_Type is record
      Parsed   : Natural := 0;
      Applied  : Natural := 0;
      Warnings : Natural := 0;
      Errors   : Natural := 0;
   end record;

   procedure Set_Custom_Text_Filters (Text_Filters : not null Filters_Access);
   --  Set custom text filters to be applied during the Apply process.

   --!pp off
   function Apply (
      Filters : String;
      Value   : String;
      Output  : IO.File_Type;
      Summary : out Results_Type;
      Errors  : Boolean := True
   ) return UString;
   --!pp on
   --
   --  Recursively apply all Filters to Value, using Output file for
   --  paragraph-formatting filters. Results contain the summary of the
   --  operation.
   --
   --  If Errors is False, then undefined filters or erroneously specified
   --  text filters are considered only as warnings. Otherwise, are considered
   --  as errors and the process is interrupted when the first error is
   --  encountered.

private

   package Reg renames GNAT.Regpat;

   Text_Filter_Matcher : Reg.Pattern_Matcher (256);

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

   function Parse
     (Filters : UString; Tail : out UString) return Text_Filter_Parsed;

   function Apply (Filter : Text_Filter_Parsed; S : UString) return UString;

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

end Text_Filters;
