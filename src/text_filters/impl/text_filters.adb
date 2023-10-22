-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Regpat;

with Simple_Logging;

with Predefined_Text_Filters; use Predefined_Text_Filters;

package body Text_Filters is

   package Log renames Simple_Logging;
   package Reg renames GNAT.Regpat;

   Text_Filter_Matcher : Reg.Pattern_Matcher (256);
   Custom_Filters      : Filters_Access := null;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filters : UString; Tail : out UString)
      return Text_Filter_Parsed is separate;

   -----------
   -- Apply --
   -----------

   function Apply
     (Filter : Text_Filter_Parsed; S : UString) return UString is separate with
     Pre => (Filter.Kind /= filter_none and Filter.Kind /= filter_error);

   -----------------------------
   -- Set_Custom_Text_Filters --
   -----------------------------

   procedure Set_Custom_Text_Filters (Text_Filters : Filters_Access) is
   begin
      Custom_Filters := Text_Filters;
   end Set_Custom_Text_Filters;

   -----------
   -- Apply --
   -----------

   --!pp off
   function Apply (
      Filters        :     String;
      Value          :     String;
      Output         :     IO.File_Type;
      Summary        : out Results_Type;
      Abort_On_Error :     Boolean := True
   )  return UString
   --!pp on

   is
      pragma Unreferenced
        (Output);  --  !TODO To be used with paragraph filters

      Filter_Parsed : Text_Filter_Parsed;

      Result : UString := To_Unbounded_String (Value);
      Filter : UString := To_Unbounded_String (Filters);
      Tail   : UString;
   begin
      Summary := (others => 0);

      Log.Debug ("Abort_On_Error : " & Abort_On_Error'Image);

      Apply_All_Text_Filters_Loop :
      loop
         Filter_Parsed := Parse (Filter, Tail);
         Summary.Found := @ + 1;

         if Filter_Parsed.Kind = filter_error then
            Log.Error
              ("Text Filter Error: " & To_String (Filter_Parsed.Error));
            Summary.Errors := @ + 1;
            if Abort_On_Error then
               exit Apply_All_Text_Filters_Loop;
            end if;
         else
            Result          := Apply (Filter_Parsed, Result);
            Filter          := Tail;
            Summary.Applied := @ + 1;
         end if;

         Log.Debug ("Filter_Parsed : " & Filter_Parsed'Image);
         Log.Debug ("Tail          : '" & Tail'Image & "'");
         Log.Debug ("Result        : '" & To_String (Result) & "'");

         exit Apply_All_Text_Filters_Loop when Tail = Null_Unbounded_String;
      end loop Apply_All_Text_Filters_Loop;

      return Result;
   end Apply;

begin

   Text_Filter_Matcher.Compile ("^/([a-zA-Z0-9])((?:[^/]|//)*)(.*)$");
   --                              |     1     ||      2     || 3|
   --                              '-----------''------------''--'
   --  (1) --> First text filter
   --  (2) --> Text filter arguments; '//' is the escaped '/'
   --  (3) --> Empty or additional text filters

end Text_Filters;
