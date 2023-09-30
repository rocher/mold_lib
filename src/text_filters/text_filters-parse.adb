-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

-----------
-- Parse --
-----------

separate (Text_Filters)
function Parse
  (Filters : UString; Tail : out UString) return Text_Filter_Parsed
is
   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);

   function UStr (S : String) return UString renames To_Unbounded_String;

   Arg         : Character;
   Filter_Kind : Character;
   Matches     : Reg.Match_Array (0 .. 2);
begin
   Text_Filter_Matcher.Match (To_String (Filters), Matches, 1);

   if Matches (2).First < Matches (2).Last then
      Tail := Filters.Unbounded_Slice (Matches (2).First, Matches (2).Last);
   else
      Tail := Null_Unbounded_String;
   end if;

   Log.Debug ("Filters    : """ & To_String (Filters) & """");
   Log.Debug ("Element(1) : " & Filters.Element (Matches (1).First + 1));
   Log.Debug ("Tail       : """ & To_String (Tail) & """");
   Log.Debug ("Matches    : " & Matches'Image);

   return Text_Filter : Text_Filter_Parsed do
      Filter_Kind := Filters.Element (Matches (1).First + 1);
      case Filter_Kind is

         when '0' .. '9' =>
            Text_Filter.Number := Integer'Value (Filter_Kind & "");
            if Custom_Filters = null
              or else Custom_Filters.all (Text_Filter.Number) = null
            then
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Null custom filter '" & Filter_Kind & "'");
            else
               Text_Filter.Kind := filter_custom;
            end if;

         when 'T' =>
            if Matches (1).Last = Matches (1).First + 2 then
               Arg := Filters.Element (Matches (1).Last);
               case Arg is
                  when 'l' =>
                     Text_Filter.Kind := filter_trim_left;
                  when 'r' =>
                     Text_Filter.Kind := filter_trim_right;
                  when 'b' =>
                     Text_Filter.Kind := filter_trim_both;
                  when 's' =>
                     Text_Filter.Kind := filter_trim_squash;
                  when 'a' =>
                     Text_Filter.Kind := filter_trim_all;
                  when others =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr ("Invalid argument '" & Arg & "' in filter 'T'");
               end case;
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'T'");
            end if;

         when 'X' =>
            Text_Filter.Kind := filter_remove_blanks;

         when 'r' =>
            if Matches (1).Last = Matches (1).First + 4 then
               Arg := Filters.Element (Matches (1).First + 2);
               case Arg is
                  when 'a' =>
                     Text_Filter.Kind := filter_replace_all;
                  when 'f' =>
                     Text_Filter.Kind := filter_replace_first;
                  when 'l' =>
                     Text_Filter.Kind := filter_replace_last;
                  when others =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr
                         ("Invalid substitution '" & Arg & "' in filter 'r'");
               end case;
               Text_Filter.Src := Filters.Element (Matches (1).Last - 1);
               Text_Filter.Dst := Filters.Element (Matches (1).Last);
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'r'");
            end if;

         when 's' =>
            if Matches (1).Last = Matches (1).First + 2 then
               Text_Filter.Kind := filter_sequence;
               Text_Filter.Dst  := Filters.Element (Matches (1).Last);
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 's'");
            end if;

         when 'D' =>
            if Matches (1).Last = Matches (1).First + 2 then
               Text_Filter.Kind := filter_delete_all;
               Text_Filter.Src  := Filters.Element (Matches (1).Last);
               Text_Filter.Dst  := Filters.Element (Matches (1).Last);
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'D'");
            end if;

         when 'p' =>
            if Matches (1).Last >= Matches (1).First + 4 then
               Text_Filter.Kind := filter_padding;

               --  <DIR>
               Arg := Filters.Element (Matches (1).First + 2);
               case Arg is
                  when 'l' =>
                     Text_Filter.Dir := left;
                  when 'r' =>
                     Text_Filter.Dir := right;
                  when others =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr ("Invalid direction '" & Arg & "' in filter 'p'");
               end case;

               --  <CHAR>
               Text_Filter.Src := Filters.Element (Matches (1).First + 3);
               declare
                  Text  : constant String :=
                    Filters.Slice (Matches (1).First + 4, Matches (1).Last);
                  Width : Natural;
                  Last  : Natural;
               begin
                  Natural_IO.Get (Text, Width, Last);
                  Text_Filter.Number := Width;
               exception
                  when Ada.Text_IO.Data_Error =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr ("Invalid number '" & Text & "' in filter 'p'");
               end;
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'p'");
            end if;

         when 'W' =>
            if Matches (1).Last >= Matches (1).First + 2 then
               Text_Filter.Kind := filter_truncate;
               declare
                  Text  : constant String :=
                    Filters.Slice (Matches (1).First + 2, Matches (1).Last);
                  Width : Natural;
                  Last  : Natural;
               begin
                  Natural_IO.Get (Text, Width, Last);
                  Text_Filter.Number := Width;
               exception
                  when Ada.Text_IO.Data_Error =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr ("Invalid number '" & Text & "' in filter 'W'");
               end;
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'W'");
            end if;

         when 'c' =>
            if Matches (1).Last = Matches (1).First + 2 then
               Arg := Filters.Element (Matches (1).Last);
               case Arg is
                  when 'l' =>
                     Text_Filter.Kind := filter_case_lowercase;
                  when 'c' =>
                     Text_Filter.Kind := filter_case_capitals;
                  when 'u' =>
                     Text_Filter.Kind := filter_case_uppercase;
                  when others =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr ("Invalid case '" & Arg & "' in filter 'c'");
               end case;
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'c'");
            end if;

         when 'n' =>
            if Matches (1).Last = Matches (1).First + 2 then
               Arg := Filters.Element (Matches (1).Last);
               case Arg is
                  when 'f' =>
                     Text_Filter.Kind := filter_style_flat_case;
                  when 'c' =>
                     Text_Filter.Kind := filter_style_lower_camel_case;
                  when 'C' =>
                     Text_Filter.Kind := filter_style_upper_camel_case;
                  when 'U' =>
                     Text_Filter.Kind := filter_style_uppercase;
                  when 's' =>
                     Text_Filter.Kind := filter_style_snake_case;
                  when 'S' =>
                     Text_Filter.Kind := filter_style_camel_snake_case;
                  when 'i' =>
                     Text_Filter.Kind := filter_style_title_case;
                  when 'A' =>
                     Text_Filter.Kind := filter_style_all_caps;
                  when 'd' =>
                     Text_Filter.Kind := filter_style_dash_case;
                  when 't' =>
                     Text_Filter.Kind := filter_style_train_case;
                  when 'T' =>
                     Text_Filter.Kind := filter_style_train_uppercase;
                  when others =>
                     Text_Filter.Kind  := filter_error;
                     Text_Filter.Error :=
                       UStr ("Invalid argument '" & Arg & "'in filter 'n'");
               end case;
            end if;

         when others =>
            Text_Filter.Kind := filter_none;
      end case;
   end return;
end Parse;
