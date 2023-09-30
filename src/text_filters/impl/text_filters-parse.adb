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

   Kind    : Character;
   Argc    : Natural;
   Args    : UString;
   Arg     : Character;
   Matches : Reg.Match_Array (0 .. 3);

   function UStr (S : String) return UString renames To_Unbounded_String;

   procedure Get_Args is
      Slash : Boolean := False;
   begin
      Argc := 0;
      Args := Null_Unbounded_String;
      if Matches (2).First <= Matches (2).Last then
         for I in Matches (2).First .. Matches (2).Last loop
            if Slash then
               Slash := False;
            else
               Args.Append (Filters.Element (I));
               Argc  := Argc + 1;
               Slash := (Filters.Element (I) = '/');
            end if;
         end loop;
      end if;
   end Get_Args;

begin
   Text_Filter_Matcher.Match (To_String (Filters), Matches, 1);

   if Matches (3).First < Matches (3).Last then
      Tail := Filters.Unbounded_Slice (Matches (3).First, Matches (3).Last);
   else
      Tail := Null_Unbounded_String;
   end if;

   Log.Debug ("Filters     : """ & To_String (Filters) & """");
   --  Log.Debug
   --    ("Matches(0)  : " & Matches (0).First'Image & ", " &
   --     Matches (0).Last'Image);
   --  Log.Debug
   --    ("Matches(1)  : " & Matches (1).First'Image & ", " &
   --     Matches (1).Last'Image);
   --  Log.Debug
   --    ("Matches(2)  : " & Matches (2).First'Image & ", " &
   --     Matches (2).Last'Image);
   --  Log.Debug
   --    ("Matches(3)  : " & Matches (3).First'Image & ", " &
   --     Matches (3).Last'Image);
   Log.Debug ("Filter kind : " & Filters.Element (Matches (1).First));
   Log.Debug
     ("Filter args : " & Filters.Slice (Matches (2).First, Matches (2).Last));
   Log.Debug ("Tail        : """ & To_String (Tail) & """");
   Log.Debug ("Matches     : " & Matches'Image);

   return Text_Filter : Text_Filter_Parsed do
      Kind := Filters.Element (Matches (1).First);
      case Kind is

         when '0' .. '9' =>
            Text_Filter.Number := Integer'Value (Kind & "");
            if Custom_Filters = null
              or else Custom_Filters.all (Text_Filter.Number) = null
            then
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error := UStr ("Null custom filter '" & Kind & "'");
            else
               Text_Filter.Kind := filter_custom;
            end if;

         when 'T' =>
            Get_Args;
            if Argc = 1 then
               Arg := Args.Element (1);
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
            Get_Args;
            if Argc = 3 then
               Arg := Args.Element (1);
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
               Text_Filter.Src := Args.Element (2);
               Text_Filter.Dst := Args.Element (3);
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'r'");
            end if;

         when 's' =>
            Get_Args;
            if Argc = 1 then
               Text_Filter.Kind := filter_sequence;
               Text_Filter.Dst  := Args.Element (1);
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 's'");
            end if;

         when 'D' =>
            Get_Args;
            if Argc = 1 then
               Text_Filter.Kind := filter_delete_all;
               Text_Filter.Src  := Args.Element (1);
               Text_Filter.Dst  := Args.Element (1);
            else
               Text_Filter.Kind  := filter_error;
               Text_Filter.Error :=
                 UStr ("Invalid number or arguments in filter 'D'");
            end if;

         when 'p' =>
            Get_Args;
            if Argc >= 3 then
               Text_Filter.Kind := filter_padding;

               --  <DIR>
               Arg := Args.Element (1);
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
               Text_Filter.Src := Args.Element (2);
               declare
                  Text  : constant String := Args.Slice (3, Args.Length);
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
            Get_Args;
            if Argc >= 1 then
               Text_Filter.Kind := filter_truncate;
               declare
                  Text  : constant String := Args.Slice (1, Args.Length);
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
            Get_Args;
            if Argc = 1 then
               Arg := Args.Element (1);
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
            Get_Args;
            if Argc = 1 then
               Arg := Args.Element (1);
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
