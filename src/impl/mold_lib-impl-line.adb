-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold_Lib.Impl.Variables;
with Text_Filters;

package body Mold_Lib.Impl.Line is

   use all type Reg.Match_Location;

   -------------
   -- Replace --
   -------------

   function Replace
     (Line : String; Number : Natural; Output : IO.File_Type) return String
   is
      Matches     : Reg.Match_Array (0 .. 4);
      New_Line    : Unbounded_String := To_Unbounded_String ("");
      Current     : Natural          := Line'First;
      Has_Matches : Boolean          := False;
   begin

      loop
         Variable_Matcher.Match (Line, Matches, Current);
         exit when Matches (0) = Reg.No_Match;

         Has_Matches := True;
         Inc_Result (Variables_Found);

         declare
            Pre_Text : constant String :=
              Line (Matches (1).First .. Matches (1).Last);

            Var_Mold : constant String :=
              Line (Matches (2).First .. Matches (2).Last);

            Var_All_Name : constant String :=
              Line (Matches (3).First .. Matches (3).Last);

            Is_Mandatory : constant Boolean :=
              (Var_All_Name (Var_All_Name'First) =
               Mandatory_Replacement_Prefix);

            Is_Optional : constant Boolean :=
              (Var_All_Name (Var_All_Name'First) =
               Optional_Replacement_Prefix);

            Var_Name : constant String :=
              (if Is_Mandatory or Is_Optional then
                 Var_All_Name (Var_All_Name'First + 1 .. Var_All_Name'Last)
               else Var_All_Name);

            Filters : constant String :=
              (if Matches (4).First > 0 then
                 Line (Matches (4).First .. Matches (4).Last)
               else "");

            Var_Value : constant String := Impl.Variables.Get_Value (Var_Name);

            Variable_Undefined : constant Boolean := (Var_Value = "");
         begin

            --  Log.Debug ("Pre_Text    : '" & Pre_Text & "'");
            --  Log.Debug ("Var_Mold    : '" & Var_Mold & "'");
            --  Log.Debug ("Var_All_Name: '" & Var_All_Name & "'");
            --  Log.Debug ("Var_Name    : '" & Var_Name & "'");
            --  Log.Debug ("Filters     : '" & Filters & "'");

            New_Line.Append (Pre_Text);

            if Variable_Undefined then
               Inc_Result (Variables_Undefined);
               declare
                  LIN     : constant String := Number'Image;
                  COL     : constant String := Matches (2).First'Image;
                  Message : constant String :=
                    "Undefined variable '" & Var_Name & "' in " &
                    Args.Source.all & ":" & LIN (2 .. LIN'Last) & ":" &
                    COL (2 .. COL'Last);
               begin
                  if Is_Mandatory then
                     Inc_Result (Variables_Ignored);
                     Inc_Result (Replacement_Errors);
                     New_Line.Append (Var_Mold);
                     Log.Error (Message);
                     Args.Errors := @ + 1;
                  elsif Is_Optional then
                     Inc_Result (Variables_Emptied);
                  else  --  Is Normal
                     if Args.Settings.Undefined_Variable_Alert = Warning then
                        Inc_Result (Replacement_Warnings);
                        Log.Warning (Message);
                     elsif Args.Settings.Undefined_Variable_Alert = Error then
                        Log.Error (Message);
                        Args.Errors := @ + 1;
                     end if;
                     if Args.Settings.Undefined_Variable_Action = Ignore then
                        Inc_Result (Variables_Ignored);
                        New_Line.Append (Var_Mold);
                     else
                        Inc_Result (Variables_Emptied);
                     end if;
                  end if;
               end;
            else  --  variable defined
               Inc_Result (Variables_Replaced);
               if Filters = "" then
                  New_Line.Append (Var_Value);
               else
                  Log.Debug ("Applying filters");
                  declare
                     Results      : Text_Filters.Results_Type;
                     Var_Filtered : constant Unbounded_String :=
                       Text_Filters.Apply
                         (Filters, Var_Value, Output, Results,
                          Abort_On_Error =>
                            Args.Settings.Undefined_Filter_Alert = Error);
                  begin
                     Inc_Result (Filters_Found, Results.Found);
                     Inc_Result (Filters_Applied, Results.Applied);
                     if Args.Settings.Undefined_Filter_Alert = Error then
                        Inc_Result (Replacement_Errors, Results.Errors);
                     else
                        Inc_Result (Replacement_Warnings, Results.Errors);
                     end if;
                     if Results.Errors = 0
                       or else Args.Settings.Undefined_Filter_Alert = Warning
                     then
                        New_Line.Append (Var_Filtered);
                     else
                        Args.Errors := @ + Results.Errors;
                     end if;
                  end;
               end if;
            end if;
         end;

         Current := Matches (0).Last + 1;
      end loop;

      if Has_Matches then
         New_Line.Append (Line (Current .. Line'Last));
         return To_String (New_Line);
      else
         return Line;
      end if;
   end Replace;

end Mold_Lib.Impl.Line;
