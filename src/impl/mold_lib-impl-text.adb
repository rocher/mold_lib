-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold_Lib.Impl.Variables;
with Text_Filters;

package body Mold_Lib.Impl.Text is

   use all type Reg.Match_Location;

   -------------
   -- Replace --
   -------------

   --!pp off
   function Replace (
      Text    :     String;
      Entity  :     Entity_Kind;
      Line    :     Natural;
      Name    :     String;
      Success : out Boolean;
      Output  :     IO.File_Access := null  --  RFU
   ) return String
   --!pp on

   is
      Matches     : Reg.Match_Array (0 .. 4);
      New_Text    : Unbounded_String := To_Unbounded_String ("");
      Current     : Natural          := Text'First;
      Has_Matches : Boolean          := False;

      procedure Local_Inc_Result
        (Field : Results_Fields; Amount : Natural := 1)
      is
      --  Local version of Inc_Result to increment only results when Entity
      --  is a file
      begin
         if Entity = file then
            Inc_Result (Field, Amount);
         end if;
      end Local_Inc_Result;
   begin
      Success := True;

      loop
         Variable_Matcher.Match (Text, Matches, Current);
         exit when Matches (0) = Reg.No_Match;

         Has_Matches := True;
         Local_Inc_Result (Variables_Found);

         declare
            Pre_Text : constant String :=
              Text (Matches (1).First .. Matches (1).Last);

            Var_Mold : constant String :=
              Text (Matches (2).First .. Matches (2).Last);

            Var_All_Name : constant String :=
              Text (Matches (3).First .. Matches (3).Last);

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
                 Text (Matches (4).First .. Matches (4).Last)
               else "");

            Var_Value : constant String := Impl.Variables.Get_Value (Var_Name);

            Variable_Undefined : constant Boolean := (Var_Value = "");

            LIN : constant String := Line'Image;
            COL : constant String := Matches (2).First'Image;
         begin

            --  Log.Debug ("Pre_Text    : '" & Pre_Text & "'");
            --  Log.Debug ("Var_Mold    : '" & Var_Mold & "'");
            --  Log.Debug ("Var_All_Name: '" & Var_All_Name & "'");
            --  Log.Debug ("Var_Name    : '" & Var_Name & "'");
            --  Log.Debug ("Filters     : '" & Filters & "'");

            New_Text.Append (Pre_Text);

            if Variable_Undefined then
               Local_Inc_Result (Variables_Undefined);
               declare
                  Message : constant String :=
                    "Undefined variable '" & Var_Name & "' in " &
                    (if Entity = file then
                       "file " & Args.Source.all & ":" & LIN (2 .. LIN'Last) &
                       ":" & COL (2 .. COL'Last)
                     else "variable '" & Name & "'");
               begin
                  if Is_Mandatory then
                     Local_Inc_Result (Variables_Ignored);
                     New_Text.Append (Var_Mold);
                     Log.Error (Message);
                     Success := False;
                  elsif Is_Optional then
                     Local_Inc_Result (Variables_Emptied);
                  else  --  Is Normal
                     if Args.Settings.Undefined_Alert = Warning then
                        Local_Inc_Result (Warnings);
                        Log.Warning (Message);
                     elsif Args.Settings.Undefined_Alert = Error then
                        Log.Error (Message);
                        Success := False;
                     end if;
                     if Args.Settings.Undefined_Action = Ignore then
                        Local_Inc_Result (Variables_Ignored);
                        New_Text.Append (Var_Mold);
                     else
                        Local_Inc_Result (Variables_Emptied);
                     end if;
                  end if;
               end;
            else
               --  variable defined

               if Entity = variable and then Var_Name = Name then
                  if Line = 1 then
                     --  Error: found recursive definition of variable
                     Log.Error
                       ("Recursive definition of variable '" & Name & "'");
                  else
                     --  Error: found cyclic definition of variable
                     Log.Error
                       ("Cyclic definition (loop" & Line'Image &
                        ") of variable '" & Name & "'");
                  end if;
                  Success := False;
               end if;

               if Filters = "" then
                  Local_Inc_Result (Variables_Replaced);
                  New_Text.Append (Var_Value);
               else
                  Log.Debug ("Applying filters");
                  declare
                     Var_Filter_Applied : constant Unbounded_String :=
                       Text_Filters.Apply (Filters, Var_Value, Output);
                  begin
                     if Var_Filter_Applied = Null_Unbounded_String then
                        if Args.Settings.Undefined_Action = Ignore then
                           Local_Inc_Result (Variables_Ignored);
                           New_Text.Append (Var_Mold);
                        else
                           Local_Inc_Result (Variables_Emptied);
                        end if;
                        if Args.Settings.Undefined_Alert = Error then
                           Log.Error
                             ("Invalid text filter '" & Filters & "' in " &
                              Args.Source.all & ":" & LIN (2 .. LIN'Last) &
                              ":" & COL (2 .. COL'Last));
                           Success := False;
                        elsif Args.Settings.Undefined_Alert = Warning then
                           Local_Inc_Result (Warnings);
                           Log.Warning
                             ("Invalid text filter '" & Filters & "' in " &
                              Args.Source.all & ":" & LIN (2 .. LIN'Last) &
                              ":" & COL (2 .. COL'Last));
                        end if;
                     else
                        if Entity = file then
                           --  do not count replacements in variables
                           Local_Inc_Result (Variables_Replaced);
                        end if;
                        New_Text.Append (Var_Filter_Applied);
                     end if;
                  end;
               end if;
            end if;
         end;

         if not Success then
            return "";
         end if;

         Current := Matches (0).Last + 1;
      end loop;

      if Has_Matches then
         New_Text.Append (Text (Current .. Text'Last));
         return To_String (New_Text);
      else
         return Text;
      end if;
   end Replace;

end Mold_Lib.Impl.Text;
