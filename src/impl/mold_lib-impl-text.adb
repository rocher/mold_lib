-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with GNAT.Calendar.Time_IO;

with Mold_Lib_Config;
with Mold_Lib.Impl.Variables;
with Text_Filters;
with Log_Exceptions; use Log_Exceptions;
with Log_Wrapper;    use Log_Wrapper;

package body Mold_Lib.Impl.Text is

   use all type Reg.Match_Location;

   ----------------------
   -- Local_Inc_Result --
   ----------------------
   --  Local version of Inc_Result to increment only results when Entity
   --  is a file

   procedure Local_Inc_Result
     (Entity : Entity_Kind; Field : Results_Fields; Amount : Natural := 1) is
   begin
      if Entity = text_line then
         Inc_Result (Field, Amount);
      end if;
   end Local_Inc_Result;

   -------------------------------
   -- Manage_Undefined_Variable --
   -------------------------------

   procedure Manage_Undefined_Variable
     (Name         : String;
      Var_Name     : String;
      Var_Mold     : String;
      Entity       : Entity_Kind;
      Is_Mandatory : Boolean;
      Is_Optional  : Boolean;
      New_Text     : in out Unbounded_String;
      LIN          : String;
      COL          : String;
      Success      : in out Boolean)
   is
      Message : constant String :=
        "Undefined variable '"
        & Var_Name
        & "' in "
        & (if Entity = text_line
           then
             "file "
             & Args.Source.all
             & ":"
             & LIN (LIN'First + 1 .. LIN'Last)
             & ":"
             & COL (COL'First + 1 .. COL'Last)
           else "variable '" & Name & "'");
   begin
      Local_Inc_Result (Entity, Variables_Undefined);
      if Is_Mandatory then
         Local_Inc_Result (Entity, Variables_Ignored);
         New_Text.Append (Var_Mold);
         Log.Error (Message);
         Success := False;
      elsif Is_Optional then
         Local_Inc_Result (Entity, Variables_Emptied);
      else
         --  Is Normal
         if Args.Settings.On_Undefined = Ignore then
            Local_Inc_Result (Entity, Variables_Ignored);
            New_Text.Append (Var_Mold);
         elsif Args.Settings.On_Undefined = Warning then
            Local_Inc_Result (Entity, Variables_Emptied);
            Local_Inc_Result (Entity, Warnings);
            Log.Warning (Message);
         elsif Args.Settings.On_Undefined = Error then
            Log.Error (Message);
            Success := False;
         end if;
      end if;
   end Manage_Undefined_Variable;

   --------------------------
   -- Manage_Date_Variable --
   --------------------------

   procedure Manage_Date_Variable
     (Var_Name             : String;
      Var_Mold             : String;
      Var_Value            : in out Unbounded_String;
      Entity               : Entity_Kind;
      New_Text             : in out Unbounded_String;
      LIN                  : String;
      COL                  : String;
      Valid_Predefined_Var : in out Boolean;
      Success              : in out Boolean)
   is
      Format : constant GNAT.Calendar.Time_IO.Picture_String :=
        GNAT.Calendar.Time_IO.Picture_String
          (Slice
             (To_Unbounded_String (Var_Name),
              11, --  skip 'mold-date-' prefix (10)
              Var_Name'Length));

      Format_Str : constant String := String (Format);

      -------------------------
      -- Manage_Format_Error --
      -------------------------

      procedure Manage_Format_Error (Result : String) is
      begin
         Valid_Predefined_Var := False;
         if Args.Settings.On_Undefined = Ignore then
            Local_Inc_Result (Entity, Variables_Ignored);
            New_Text.Append (Var_Mold);
         elsif Args.Settings.On_Undefined = Warning then
            Local_Inc_Result (Entity, Variables_Emptied);
            Local_Inc_Result (Entity, Warnings);
            Log.Warning
              ("Invalid date format '"
               & String (Format)
               & "' in "
               & Args.Source.all
               & ":"
               & LIN (LIN'First + 1 .. LIN'Last)
               & ":"
               & COL (COL'First + 1 .. COL'Last));
         else
            Log.Error
              ("Invalid date format '"
               & String (Format)
               & "' in "
               & Args.Source.all
               & ":"
               & LIN (LIN'First + 1 .. LIN'Last)
               & ":"
               & COL (COL'First + 1 .. COL'Last));
            Log.Error ("Result is " & Result);
            Success := False;
         end if;
      end Manage_Format_Error;

   begin
      if Format_Str = "ISO_Time" then
         Var_Value :=
           To_Unbounded_String
             ((GNAT.Calendar.Time_IO.Image
                 (Args.Invocation_Time, "%Y-%m-%dT%H:%M:%S%:::z")));
      elsif Format_Str = "ISO_Date" then
         Var_Value :=
           To_Unbounded_String
             ((GNAT.Calendar.Time_IO.Image
                 (Args.Invocation_Time, "%Y-%m-%d")));
      elsif Format_Str = "US_Date" then
         Var_Value :=
           To_Unbounded_String
             ((GNAT.Calendar.Time_IO.Image
                 (Args.Invocation_Time, "%m/%d/%y")));
      elsif Format_Str = "EU_Date" then
         Var_Value :=
           To_Unbounded_String
             ((GNAT.Calendar.Time_IO.Image
                 (Args.Invocation_Time, "%d/%m/%y")));
      else
         declare
            Result : constant String :=
              GNAT.Calendar.Time_IO.Image (Args.Invocation_Time, Format);
         begin
            Log_Debug ("Result: " & Result);
            Log_Debug ("Format_Str: " & Format_Str);

            if Format_Str /= Result then
               Var_Value := To_Unbounded_String (Result);
            else
               Manage_Format_Error (Result);
            end if;
         end;
      end if;

   exception
      when E : GNAT.Calendar.Time_IO.Picture_Error =>
         Manage_Format_Error ("unknown");
         Log_Exception (E);
         pragma Annotate (Xcov, Exempt_Off);
   end Manage_Date_Variable;

   --------------------------------
   -- Manage_Predefined_Variable --
   --------------------------------

   procedure Manage_Predefined_Variable
     (Var_Name             : String;
      Var_Mold             : String;
      Var_Value            : in out Unbounded_String;
      Entity               : Entity_Kind;
      New_Text             : in out Unbounded_String;
      LIN                  : String;
      COL                  : String;
      Valid_Predefined_Var : in out Boolean;
      Success              : in out Boolean) is
   begin
      if Var_Name = "mold-version" then
         Var_Value := To_Unbounded_String (Mold_Lib_Config.Crate_Version);

      elsif Var_Name = "mold-host-os" then
         Var_Value := To_Unbounded_String (Mold_Lib_Config.Alire_Host_OS);

      elsif Var_Name = "mold-host-arch" then
         Var_Value := To_Unbounded_String (Mold_Lib_Config.Alire_Host_Arch);

      elsif Var_Name = "mold-host-distro" then
         Var_Value := To_Unbounded_String (Mold_Lib_Config.Alire_Host_Distro);

      elsif Var_Name = "mold-build-profile" then
         Var_Value :=
           To_Unbounded_String
             (Mold_Lib_Config.Build_Profile_Kind'Image
                (Mold_Lib_Config.Build_Profile));

      elsif Index (To_Unbounded_String (Var_Name), "mold-date-") = 1 then
         Manage_Date_Variable
           (Var_Name,
            Var_Mold,
            Var_Value,
            Entity,
            New_Text,
            LIN,
            COL,
            Valid_Predefined_Var,
            Success);

      else
         Manage_Undefined_Variable
           (Name         => Name,
            Var_Name     => Var_Name,
            Var_Mold     => Var_Mold,
            Entity       => Entity,
            Is_Mandatory => True,
            Is_Optional  => False,
            New_Text     => New_Text,
            LIN          => LIN,
            COL          => COL,
            Success      => Success);
         Valid_Predefined_Var := False;
      end if;
   end Manage_Predefined_Variable;

   ---------------------------------
   -- Append_Filters_To_Variables --
   ---------------------------------

   function Append_Filters_To_Variables
     (Variables : String; Filters : String) return Unbounded_String
   with Pre => Filters /= ""
   is
      Matches : Reg.Match_Array (0 .. 4);
      Current : Natural := Variables'First;
      Result  : Unbounded_String := To_Unbounded_String ("");
   begin
      loop
         --  Loop over all matches of the regular expression in the Variables
         --  and append the filters to the variables
         Variable_Matcher.Match (Variables, Matches, Current);
         exit when Matches (0) = Reg.No_Match;

         declare
            Pre_Text : constant String :=
              Variables (Matches (1).First .. Matches (1).Last);
            --  Text before the mold variable

            Var_All_Name : constant String :=
              Variables (Matches (3).First .. Matches (3).Last);
            --  The whole name of the variable, including the prefix

            Var_Filters : constant String :=
              (if Matches (4).First > 0
               then Variables (Matches (4).First .. Matches (4).Last)
               else "");
            --  The filter of the variable (if any),
         begin
            Result.Append
              (
               --  Text before the mold variable
               Pre_Text
               --  Opening brackets
               & "{{"
               --  Name of the variable, including the prefix
               & Var_All_Name
               --  Filters, if any, of the variable
               & Var_Filters
               --  Filter to be appended to the variable
               & Filters
               --  Closing brackets
               & "}}");
            Current := Matches (0).Last + 1;
         end;
      end loop;

      --  Append the rest of the variables after the last match
      Result.Append (Variables (Current .. Variables'Last));

      return Result;
   end Append_Filters_To_Variables;

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
      Current     : Natural := Text'First;
      Has_Matches : Boolean := False;

   begin
      Success := True;

      loop
         --  Loop over all matches of the regular expression in the Text
         Variable_Matcher.Match (Text, Matches, Current);
         exit when Matches (0) = Reg.No_Match;

         Has_Matches := True;
         Local_Inc_Result (Entity, Variables_Found);

         declare
            --  Set of local variables per matched pattern, according to the
            --  regular expression used to match the variable in the file
            --  mold_lib-impl.adb, in the package initialization section.
            --  Follow the example there for more information.

            Pre_Text : constant String :=
              Text (Matches (1).First .. Matches (1).Last);
            --  Text before the mold variable

            Var_Mold : constant String :=
              Text (Matches (2).First .. Matches (2).Last);
            --  The mold variable string, including the brackets,
            --  e.g. "{{ foo }}" or "{{ #foo/s }}"

            Var_All_Name : constant String :=
              Text (Matches (3).First .. Matches (3).Last);
            --  The whole name of the variable, including the prefix
            --  (mandatory or optional), e.g. "#foo" or "foo"

            Is_Mandatory : constant Boolean :=
              (Var_All_Name (Var_All_Name'First)
               = Mandatory_Replacement_Prefix);
            --  Whether the variable is mandatory or not

            Is_Optional : constant Boolean :=
              (Var_All_Name (Var_All_Name'First)
               = Optional_Replacement_Prefix);
            --  Whether the variable is optional or not

            Var_Name : constant String :=
              (if Is_Mandatory or Is_Optional
               then Var_All_Name (Var_All_Name'First + 1 .. Var_All_Name'Last)
               else Var_All_Name);
            --  The name of the variable, without the prefix or the filter,
            --  e.g. "foo"

            Filters : constant String :=
              (if Matches (4).First > 0
               then Text (Matches (4).First .. Matches (4).Last)
               else "");
            --  The filter to apply to the variable value (if any),
            --  e.g. "/s " (including the space at the end)

            Var_Value : Unbounded_String :=
              To_Unbounded_String (Impl.Variables.Get_Value (Var_Name));
            --  The value of the variable, e.g. "bar", or the empty string if
            --  the variable is not defined

            Var_Value_Matches     : Reg.Match_Array (0 .. 4);
            Var_Value_Is_Variable : Boolean;
            --  Whether the value of the variable is a variable itself or not
            --  (e.g. "{{ #foo/s }}" where "foo" is a variable)

            Var_Is_Predefined : constant Boolean :=
              Index (To_Unbounded_String (Var_Name), "mold-") > 0;
            --  Whether the variable is a potentially predefined variable or
            --  not

            Valid_Predefined_Var : Boolean := True;
            --  Whether the predefined variable is valid or not, after
            --  processing it

            Variable_Undefined : constant Boolean :=
              (Var_Value = "") and then (not Var_Is_Predefined);
            --  Whether the variable is undefined or not

            LIN : constant String := Line'Image;
            COL : constant String := Matches (2).First'Image;
            --  Line and column of the match in the text, used for error
            --  reporting
         begin

            --  Check if the variable value is a variable itself
            --  (e.g. "{{ #foo/s }}" where "foo" is a variable)
            Variable_Matcher.Match (To_String (Var_Value), Var_Value_Matches);
            Var_Value_Is_Variable := Var_Value_Matches (0) /= Reg.No_Match;

            Log_Debug ("Entity      : " & Entity'Image);
            Log_Debug ("LIN         : " & LIN'Image);
            Log_Debug ("COL         : " & COL'Image);
            Log_Debug ("Pre_Text    : '" & Pre_Text & "'");
            Log_Debug ("Var_Mold    : '" & Var_Mold & "'");
            Log_Debug ("Var_All_Name: '" & Var_All_Name & "'");
            Log_Debug ("Var_Name    : '" & Var_Name & "'");
            Log_Debug ("Var_Value   : '" & To_String (Var_Value) & "'");
            Log_Debug
              ("Is_Variable : " & Boolean'Image (Var_Value_Is_Variable));
            Log_Debug ("Filters     : '" & Filters & "'");
            Log_Debug ("Undefined   : " & Boolean'Image (Variable_Undefined));
            Log_Debug
              ("Var_Is_Predefined: " & Boolean'Image (Var_Is_Predefined));

            --  Append the text before the variable to the new text (if any)
            New_Text.Append (Pre_Text);

            if Variable_Undefined then
               Manage_Undefined_Variable
                 (Name,
                  Var_Name,
                  Var_Mold,
                  Entity,
                  Is_Mandatory,
                  Is_Optional,
                  New_Text,
                  LIN,
                  COL,
                  Success);

               goto End_Replacement_Processing;
            end if;

            --  VARIABLE DEFINED

            --  Check for recursive or cyclic definitions
            if Entity = variable and then Var_Name = Name then
               if Line = 1 then
                  --  Error: found recursive definition of variable
                  Log.Error
                    ("Recursive definition of variable '" & Name & "'");
               else
                  --  Error: found cyclic definition of variable
                  Log.Error
                    ("Cyclic definition (loop"
                     & Line'Image
                     & ") of variable '"
                     & Name
                     & "'");
               end if;
               Success := False;
            end if;

            --  Manage date variables
            if Var_Is_Predefined then
               Manage_Predefined_Variable
                 (Var_Name,
                  Var_Mold,
                  Var_Value,
                  Entity,
                  New_Text,
                  LIN,
                  COL,
                  Valid_Predefined_Var,
                  Success);
            end if;

            --  If everything is fine, proceed with the variable replacement
            --  and filter application
            if Success and then Valid_Predefined_Var then
               if Filters = "" then
                  Local_Inc_Result (Entity, Variables_Replaced);
                  New_Text.Append (Var_Value);
               elsif Var_Value_Is_Variable then
                  --  If the value is a variable, we do not apply the filters
                  --  to it, as it is expected to be replaced later. Instead,
                  --  append the variable value, including prefix and filters,
                  --  and keep the variable filters for later.
                  declare
                     Variable_String : constant Unbounded_String :=
                       Append_Filters_To_Variables
                         (To_String (Var_Value), Filters);
                  begin
                     Log_Debug
                       ("Variable value is a variable, skipping filters in "
                        & To_String (Var_Value));
                     Log_Debug
                       ("Appending variable value with filters: "
                        & To_String (Variable_String));
                     New_Text.Append (Variable_String);
                  end;
               else
                  Log_Debug ("Applying filters");
                  declare
                     Var_Filter_Applied : constant Unbounded_String :=
                       Text_Filters.Apply
                         (Filters, To_String (Var_Value), Output);
                  begin
                     if Var_Filter_Applied = Null_Unbounded_String then
                        --  Error: filter not found or error in filter
                        --  application
                        if Args.Settings.On_Undefined = Ignore then
                           Local_Inc_Result (Entity, Variables_Ignored);
                           New_Text.Append (Var_Mold);
                        elsif Args.Settings.On_Undefined = Warning then
                           Local_Inc_Result (Entity, Variables_Emptied);
                           Local_Inc_Result (Entity, Warnings);
                           Log.Warning
                             ("Invalid text filter '"
                              & Filters
                              & "' in "
                              & Args.Source.all
                              & ":"
                              & LIN (2 .. LIN'Last)
                              & ":"
                              & COL (2 .. COL'Last));
                        elsif Args.Settings.On_Undefined = Error then
                           Log.Error
                             ("Invalid text filter '"
                              & Filters
                              & "' in "
                              & Args.Source.all
                              & ":"
                              & LIN (2 .. LIN'Last)
                              & ":"
                              & COL (2 .. COL'Last));
                           Success := False;
                        end if;
                     else
                        New_Text.Append (Var_Filter_Applied);
                        Local_Inc_Result (Entity, Variables_Replaced);
                     end if;
                  end;
               end if;
            end if;
         end;

         <<End_Replacement_Processing>>

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
