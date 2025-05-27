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
      if Entity = file then
         Inc_Result (Field, Amount);
      end if;
   end Local_Inc_Result;

   -------------------------------
   -- Manage_Undefined_Variable --
   -------------------------------

   procedure Manage_Undefined_Variable
     (Entity       : Entity_Type;
      Var_Name     : String;
      Var_Mold     : String;
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
        & (if Entity.Kind = file
           then
             "file "
             & Args.Source.all
             & ":"
             & LIN (LIN'First + 1 .. LIN'Last)
             & ":"
             & COL (COL'First + 1 .. COL'Last)
           else "variable '" & To_String (Entity.Name) & "'");
   begin
      Local_Inc_Result (Entity.Kind, Variables_Undefined);
      if Is_Mandatory then
         Local_Inc_Result (Entity.Kind, Variables_Ignored);
         New_Text.Append (Var_Mold);
         Log.Error (Message);
         Success := False;
      elsif Is_Optional then
         Local_Inc_Result (Entity.Kind, Variables_Emptied);
      else
         --  Is Normal
         if Args.Settings.On_Undefined = Ignore then
            Local_Inc_Result (Entity.Kind, Variables_Ignored);
            New_Text.Append (Var_Mold);
         elsif Args.Settings.On_Undefined = Warning then
            Local_Inc_Result (Entity.Kind, Variables_Emptied);
            Local_Inc_Result (Entity.Kind, Warnings);
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
     (Entity               : Entity_Type;
      Var_Name             : String;
      Var_Mold             : String;
      Var_Value            : in out Unbounded_String;
      Is_Mandatory         : Boolean;
      Is_Optional          : Boolean;
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
               Valid_Predefined_Var := False;
               Manage_Undefined_Variable
                 (Entity,
                  Var_Name,
                  Var_Mold,
                  Is_Mandatory,
                  Is_Optional,
                  New_Text,
                  LIN,
                  COL,
                  Success);
            end if;
         end;
      end if;

   exception
      when E : GNAT.Calendar.Time_IO.Picture_Error =>
         Valid_Predefined_Var := False;
         Manage_Undefined_Variable
           (Entity,
            Var_Name,
            Var_Mold,
            Is_Mandatory,
            Is_Optional,
            New_Text,
            LIN,
            COL,
            Success);
         if not Success then
            Log_Exception (E);
         end if;
         pragma Annotate (Xcov, Exempt_Off);
   end Manage_Date_Variable;

   --------------------------------
   -- Manage_Predefined_Variable --
   --------------------------------

   procedure Manage_Predefined_Variable
     (Entity               : Entity_Type;
      Var_Name             : String;
      Var_Mold             : String;
      Var_Value            : in out Unbounded_String;
      Is_Mandatory         : Boolean;
      Is_Optional          : Boolean;
      New_Text             : in out Unbounded_String;
      LIN                  : String;
      COL                  : String;
      Valid_Predefined_Var : in out Boolean;
      Success              : in out Boolean)
   is

      function Get_Boolean (Value : Boolean) return Unbounded_String
      is (To_Unbounded_String (Boolean'Image (Value)));
      --  Helper function to return the boolean value as a string
      --  of a predefined setting.

   begin
      --  basic information

      if Var_Name = "mold-documentation" then
         Var_Value := To_Unbounded_String (Mold_Lib.Impl.Mold_Documentation);

      elsif Var_Name = "mold-copyright" then
         Var_Value := To_Unbounded_String (Mold_Lib.Impl.Mold_Copyright);

      elsif Var_Name = "mold-license" then
         Var_Value := To_Unbounded_String (Mold_Lib.Impl.Mold_License);

      elsif Var_Name = "mold-license-spdx" then
         Var_Value := To_Unbounded_String (Mold_Lib.Impl.Mold_License_SPDX);

      elsif Var_Name = "mold-version" then
         Var_Value := To_Unbounded_String (Mold_Lib_Config.Crate_Version);

      --  host and build information

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

      --  mold settings

      elsif Var_Name = "mold-replacement-in-filenames" then
         Var_Value := Get_Boolean (Args.Settings.Replacement_In_Filenames);

      elsif Var_Name = "mold-replacement-in-variables" then
         Var_Value := Get_Boolean (Args.Settings.Replacement_In_Variables);

      elsif Var_Name = "mold-delete-source-files" then
         Var_Value := Get_Boolean (Args.Settings.Delete_Source_Files);

      elsif Var_Name = "mold-overwrite-destination-files" then
         Var_Value := Get_Boolean (Args.Settings.Overwrite_Destination_Files);

      elsif Var_Name = "mold-enable-defined-settings" then
         Var_Value := Get_Boolean (Args.Settings.Enable_Defined_Settings);

      elsif Var_Name = "mold-on-undefined" then
         Var_Value :=
           To_Unbounded_String
             (On_Undefined_Handling'Image (Args.Settings.On_Undefined));

      --  date/time variables

      elsif Index (To_Unbounded_String (Var_Name), "mold-date-") = 1 then
         Manage_Date_Variable
           (Entity,
            Var_Name,
            Var_Mold,
            Var_Value,
            Is_Mandatory,
            Is_Optional,
            New_Text,
            LIN,
            COL,
            Valid_Predefined_Var,
            Success);

      else
         Valid_Predefined_Var := False;
         Manage_Undefined_Variable
           (Entity,
            Var_Name,
            Var_Mold,
            Is_Mandatory,
            Is_Optional,
            New_Text,
            LIN,
            COL,
            Success);
      end if;
   end Manage_Predefined_Variable;

   -------------
   -- Replace --
   -------------

   function Replace
     (Text    : String;
      Entity  : Entity_Type;
      Success : out Boolean;
      Output  : IO.File_Access := null  --  RFU
      ) return String
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
         Local_Inc_Result (Entity.Kind, Variables_Found);

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

            LIN : constant String :=
              (if Entity.Kind = file then Entity.Line'Image else "");
            COL : constant String := Matches (2).First'Image;
            --  Line and column of the match in the text, used for error
            --  reporting
         begin

            --  Check if the variable value is a variable itself
            --  (e.g. "{{ #foo/s }}" where "foo" is a variable)
            Variable_Matcher.Match (To_String (Var_Value), Var_Value_Matches);
            Var_Value_Is_Variable := Var_Value_Matches (0) /= Reg.No_Match;

            Log_Debug ("Entity Kind : " & Entity.Kind'Image);
            if Entity.Kind = file then
               Log_Debug ("LIN         : " & LIN'Image);
            else
               Log_Debug ("Var. Name   : " & To_String (Entity.Name));
            end if;
            Log_Debug ("COL         : " & COL'Image);
            Log_Debug ("Pre_Text    : '" & Pre_Text & "'");
            Log_Debug ("Var_Mold    : '" & Var_Mold & "'");
            Log_Debug ("Var_All_Name: '" & Var_All_Name & "'");
            Log_Debug ("Var_Name    : '" & Var_Name & "'");
            Log_Debug ("Var_Value   : '" & To_String (Var_Value) & "'");
            Log_Debug
              ("Is_Variable : " & Boolean'Image (Var_Value_Is_Variable));
            Log_Debug ("Is_Mandatory: " & Boolean'Image (Is_Mandatory));
            Log_Debug ("Is_Optional : " & Boolean'Image (Is_Optional));
            Log_Debug ("Filters     : '" & Filters & "'");
            Log_Debug ("Undefined   : " & Boolean'Image (Variable_Undefined));
            Log_Debug
              ("Var_Is_Predefined: " & Boolean'Image (Var_Is_Predefined));

            --  Append the text before the variable to the new text (if any)
            New_Text.Append (Pre_Text);

            if Variable_Undefined then
               Manage_Undefined_Variable
                 (Entity,
                  Var_Name,
                  Var_Mold,
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
            if Entity.Kind = variable and then Var_Name = Entity.Name then
               if Entity.Loops = 1 then
                  --  Error: found recursive definition of variable
                  Log.Error
                    ("Recursive definition of variable '"
                     & To_String (Entity.Name)
                     & "'");
               else
                  --  Error: found cyclic definition of variable
                  Log.Error
                    ("Cyclic definition (loop"
                     & Entity.Loops'Image
                     & ") of variable '"
                     & To_String (Entity.Name)
                     & "'");
               end if;
               Success := False;
            end if;

            --  Manage date variables
            if Var_Is_Predefined then
               Manage_Predefined_Variable
                 (Entity,
                  Var_Name,
                  Var_Mold,
                  Var_Value,
                  Is_Mandatory,
                  Is_Optional,
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
                  Local_Inc_Result (Entity.Kind, Variables_Replaced);
                  New_Text.Append (Var_Value);
               elsif Var_Value_Is_Variable then
                  --  If the value is a variable, we do not apply the filters
                  --  to it, as it is expected to be replaced later. Instead,
                  --  append the variable value, including prefix and filters,
                  --  and keep the variable filters for later.
                  Log_Debug
                    ("Skip variable substitution and filter application");
                  New_Text.Append (Var_Value);
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
                           Local_Inc_Result (Entity.Kind, Variables_Ignored);
                           New_Text.Append (Var_Mold);
                        elsif Args.Settings.On_Undefined = Warning then
                           Local_Inc_Result (Entity.Kind, Variables_Emptied);
                           Local_Inc_Result (Entity.Kind, Warnings);
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
                        Local_Inc_Result (Entity.Kind, Variables_Replaced);
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
