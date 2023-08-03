-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions;      use AUnit.Assertions;
with Simple_Logging;

package body Support is

   function Pretty_Print
     (Errors : Natural; Results : Mold.Results_Access) return String
   is
      Fill : constant String  := "           ";
      Text : Unbounded_String := To_Unbounded_String ("");
   begin
      Text.Append (ASCII.LF & "    Errors:" & Errors'Image);
      Text.Append (ASCII.LF & "    Results:" & ASCII.LF);

      for Field in Mold.Field_Type loop
         Text.Append ("       " & Field'Image);
         Text.Append (Fill (Field'Image'Length .. Fill'Last));
         Text.Append ("=> " & Results.all (Field)'Image & ASCII.LF);
      end loop;

      return To_String (Text);
   end Pretty_Print;

   procedure Check_Results
     (Errors : Natural; Actual, Expected : Mold.Results_Access)
   is
   begin
      Simple_Logging.Detail (Pretty_Print (Errors, Actual));

      Assert
        (Errors = 0 or else Actual (Mold.Errors) = 0,
         "Incorrect Errors reported");

      for Field in Mold.Field_Type loop
         Assert
           (Actual.all (Field) = Expected.all (Field),
            "Wrong number of " & Field'Image & ": reported" &
            Actual.all (Field)'Image & ", expected" &
            Expected.all (Field)'Image);
      end loop;
   end Check_Results;

begin

   Simple_Logging.Level := Simple_Logging.Detail;

end Support;
