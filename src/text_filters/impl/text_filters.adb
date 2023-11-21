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
     Pre => (Filter.Kind /= filter_none and then Filter.Kind /= filter_error);

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
      Filters           : String;
      Value             : String;
      Output            : IO.File_Access
   )  return UString
   --!pp on

   is
      pragma Unreferenced
        (Output);  --  !TODO To be used with paragraph filters

      Parsing : Text_Filter_Parsed;
      Result  : UString := To_Unbounded_String (Value);
      Filter  : UString := To_Unbounded_String (Filters);
      Tail    : UString;
   begin
      Apply_All_Text_Filters_Loop :
      loop
         Parsing := Parse (Filter, Tail);

         if Parsing.Kind = filter_error then
            Result := Null_Unbounded_String;
            exit Apply_All_Text_Filters_Loop;
         else
            Result := Apply (Parsing, Result);
         end if;

         Log.Debug ("Filter_Parsed : " & Parsing'Image);
         Log.Debug ("Tail          : '" & Tail'Image & "'");
         Log.Debug ("Result        : '" & To_String (Result) & "'");

         exit Apply_All_Text_Filters_Loop when Tail = Null_Unbounded_String;
         Filter := Tail;
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
