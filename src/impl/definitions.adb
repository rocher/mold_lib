-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;
with TOML;
with TOML.File_IO;

with Mold_Lib.Results; use Mold_Lib.Results;

package body Definitions is

   package Log renames Simple_Logging;

   use all type Mold.Results_Access;

   ----------------------
   -- Set_Mold_Setting --
   ----------------------

   function Set_Mold_Setting
     (Key, Value : String; Settings : not null Mold.Settings_Access)
      return Boolean
   is
      Success : Boolean := True;

      -----------------
      -- Set_Boolean --
      -----------------

      procedure Set_Boolean
        (Variable : not null access Boolean; Value : String)
      is
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

      if Settings.Enable_Defined_Settings then
         if Key = "mold-replacement-in-file-names" then
            Set_Boolean (Settings.Replacement_In_File_Names'Access, Value);

         elsif Key = "mold-delete-source-files" then
            Set_Boolean (Settings.Delete_Source_Files'Access, Value);

         elsif Key = "mold-overwrite-destination-files" then
            Set_Boolean (Settings.Overwrite_Destination_Files'Access, Value);

         elsif Key = "mold-abort-on-error" then
            Set_Boolean (Settings.Abort_On_Error'Access, Value);

         elsif Key = "mold-undefined-variable-action" then
            begin
               Settings.Undefined_Variable_Action :=
                 Mold.Undefined_Variable_Actions'Value (Value);
            exception
               when Constraint_Error =>
                  Log.Error
                    ("Invalid setting value in " & Key & " = " & Value);
                  Success := False;
            end;

         elsif Key = "mold-undefined-variable-alert" then
            begin
               Settings.Undefined_Variable_Alert :=
                 Mold.Undefined_Alerts'Value (Value);
            exception
               when Constraint_Error =>
                  Log.Error
                    ("Invalid setting value in " & Key & " = " & Value);
                  Success := False;
            end;
         else
            Log.Error ("Invalid setting key in " & Key & " = " & Value);
            Success := False;
         end if;
      end if;

      if Success then
         Log.Info ("Setting applied " & Key & " = " & Value);
      end if;

      return Success;
   end Set_Mold_Setting;

   ------------------------
   -- Read_Variables_Map --
   ------------------------

   --!pp off
   function Read_Variables
   (
      Vars_File :          String;
      Settings  : not null Mold.Settings_Access;
      Results   :          Mold.Results_Access := null;
      Success   : out      Boolean
   )
   return Variables_Map
   --!pp on

   is
      use Variables_Package;

      Vars        : Variables_Map := Empty_Map;
      Read_Result : TOML.Read_Result;
   begin
      Read_Result := TOML.File_IO.Load_File (Vars_File);

      if Read_Result.Success then
         for Element of Read_Result.Value.Iterate_On_Table loop
            if Element.Key.Length >= 10
              and then Element.Key.Slice (1, 5) = Mold.Defined_Setting_Prefix
            then
               if not Set_Mold_Setting
                   (To_String (Element.Key), Element.Value.As_String, Settings)
               then
                  return Empty_Map;
               end if;
            end if;
            Vars.Include (Element.Key, Element.Value.As_Unbounded_String);

            --  Log.Debug
            --    ("defined var " & To_String (Element.Key) & " = " &
            --     Element.Value.As_String);

            Inc (Results, Mold.Variables_Defined);
         end loop;
      else
         Log.Debug ("Error reading definitions file");
      end if;

      Success := Read_Result.Success;
      return Vars;
   end Read_Variables;

end Definitions;
