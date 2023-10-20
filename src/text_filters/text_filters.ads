-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Custom_Text_Filters; use Custom_Text_Filters;
with Text_Filters_Data;   use Text_Filters_Data;

package Text_Filters is

   package IO renames Ada.Text_IO;

   type Results_Type is record
      Found   : Natural := 0;
      Applied : Natural := 0;
      Errors  : Natural := 0;
   end record;
   --  This is the summary of results during the application of a text filter
   --  (or a chain of text filters):
   --
   --     Found   : total number of filters found in a sequence
   --     Applied : number of filters applied
   --     Errors  : number of errors found

   procedure Set_Custom_Text_Filters (Text_Filters : Filters_Access);
   --  Set custom text filters to be applied during the Apply process.

   --!pp off
   function Apply (
      Filters        :     String;
      Value          :     String;
      Output         :     IO.File_Type;
      Summary        : out Results_Type;
      Abort_On_Error :     Boolean := True
   )  return UString;  --  Ada.Strings.Unbounded.Unbounded_String
   --!pp on
   --
   --  Recursively apply all Filters to Value, using Output file for
   --  paragraph-formatting filters. Results contain the summary of the
   --  operation.
   --
   --  When Abort_On_Error is True, if the process finds an undefined text
   --  filter or erroneously specified, then the process stops immediately and
   --  reports an error. Otherwise, the process continues with the remaining
   --  filters and these erroneous filters are simply skipped but reported as
   --  errors.

end Text_Filters;
