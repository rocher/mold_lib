-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Custom_Text_Filters; use Custom_Text_Filters;
with Text_Filters_Data;   use Text_Filters_Data;

package Text_Filters is

   package IO renames Ada.Text_IO;

   procedure Set_Custom_Text_Filters (Text_Filters : Filters_Access);
   --  Set custom text filters to be applied during the Apply process.

   --!pp off
   function Apply (
      Filters          : String;
      Value            : String;
      Output           : IO.File_Access
   )  return UString;  --  Ada.Strings.Unbounded.Unbounded_String
   --!pp on
   --
   --  Recursively apply all Filters to Value, using Output file for
   --  paragraph-formatting filters. Results contain the summary of the
   --  operation.
   --
   --  In the presence of undefined text filters (or erroneously specified),
   --  the function returns Null_Unbounded_String.

   --  If there are several filters in sequence, e.g. '/Ta/s/0', the filter
   --  application is "unique". That is, if any of the multiple filters is
   --  undefined, then no filter is applied.

end Text_Filters;
