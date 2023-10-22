-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Log_Exceptions; use Log_Exceptions;

with TOML;
with TOML.File_IO;

package body Mold_Lib.Impl.Variables is

   ----------------------
   -- Set_Mold_Setting --
   ----------------------

   function Set_Mold_Setting (Key, Value : String) return Boolean is

      Success : Boolean := True;

      -----------------
      -- Set_Boolean --
      -----------------

      procedure Set_Boolean (Variable : not null access Boolean) is
      begin
         case Value is
            when "TRUE" | "True" | "true" =>
               Variable.all := True;
            when "FALSE" | "False" | "false" =>
               Variable.all := False;
            when others =>
               Log.Error ("Invalid setting value in " & Key & " = " & Value);
               Success := False;
         end case;
      end Set_Boolean;

   begin
      if Key = "mold-replacement-in-file-names" then
         Set_Boolean (Args.Settings.Replacement_In_File_Names'Access);

      elsif Key = "mold-delete-source-files" then
         Set_Boolean (Args.Settings.Delete_Source_Files'Access);

      elsif Key = "mold-overwrite-destination-files" then
         Set_Boolean (Args.Settings.Overwrite_Destination_Files'Access);

      elsif Key = "mold-abort-on-error" then
         Set_Boolean (Args.Settings.Abort_On_Error'Access);

      elsif Key = "mold-undefined-variable-action" then
         begin
            Args.Settings.Undefined_Variable_Action :=
              Undefined_Variable_Actions'Value (Value);
         exception
            when E : Constraint_Error =>
               Log_Exception
                 (E, "Invalid setting value " & Key & " = " & Value);
               Success := False;
         end;

      elsif Key = "mold-undefined-variable-alert"
        or else Key = "mold-undefined-filter-alert"
      then
         declare
            Undefined_Alert : Undefined_Alerts;
         begin
            Undefined_Alert := Undefined_Alerts'Value (Value);
            if Key = "mold-undefined-variable-alert" then
               Args.Settings.Undefined_Variable_Alert := Undefined_Alert;
            else
               Args.Settings.Undefined_Filter_Alert := Undefined_Alert;
            end if;
         exception
            when E : Constraint_Error =>
               Log_Exception
                 (E, "Invalid setting value " & Key & " = " & Value);
               Success := False;
         end;
      else
         Log.Error ("Invalid setting key in " & Key & " = " & Value);
         Success := False;
      end if;

      if Success then
         Log.Info ("Setting applied " & Key & " = " & Value);
      end if;

      return Success;
   end Set_Mold_Setting;

   ----------
   -- Read --
   ----------

   function Read
     (Toml_Path : String; Success : out Boolean) return Variables_Map
   is
      use Variables_Package;

      Vars        : Variables_Map := Empty_Map;
      Read_Result : TOML.Read_Result;
   begin
      Read_Result := TOML.File_IO.Load_File (Toml_Path);

      if Read_Result.Success then
         for Element of Read_Result.Value.Iterate_On_Table loop
            if Args.Settings.Enable_Defined_Settings
              and then Element.Key.Length >= 10
              and then Element.Key.Slice (1, 5) = Defined_Setting_Prefix
            then
               if not Set_Mold_Setting
                   (To_String (Element.Key), Element.Value.As_String)
               then
                  Success := False;
                  return Empty_Map;
               end if;
            end if;
            Vars.Include (Element.Key, Element.Value.As_Unbounded_String);

            --  Log.Debug
            --    ("defined var " & To_String (Element.Key) & " = " &
            --     Element.Value.As_String);

            Inc_Result (Variables_Defined);
         end loop;
      else
         Log.Debug ("Error reading variables file");
      end if;

      Success := Read_Result.Success;
      return Vars;
   end Read;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Variable : String) return String is
      use Variables_Package;
      Ref : constant Cursor :=
        Args.Variables.Find (To_Unbounded_String (Variable));
   begin
      if Ref = No_Element then
         Log.Debug ("Unmapped variable " & Variable);
         return "";
      else
         return To_String (Element (Ref));
      end if;
   end Get_Value;

end Mold_Lib.Impl.Variables;
