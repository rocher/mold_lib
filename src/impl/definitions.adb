-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with TOML;
with TOML.File_IO;
with Simple_Logging;

with Directory;
with File;
with Results; use Results;

package body Definitions is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;
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

      if Settings.Defined_Settings then
         case Key is
            when "mold-rename-source" =>
               Set_Boolean (Settings.Rename_Source'Access, Value);
            when "mold-delete-source" =>
               Set_Boolean (Settings.Delete_Source'Access, Value);
            when "mold-overwrite" =>
               Set_Boolean (Settings.Overwrite'Access, Value);
            when "mold-abort-on-error" =>
               Set_Boolean (Settings.Abort_On_Error'Access, Value);

            when "mold-action" =>
               case Value is
                  when "IGNORE" | "Ignore" | "ignore" =>
                     Settings.Action := Mold.Ignore;
                  when "EMPTY" | "Empty" | "empty" =>
                     Settings.Action := Mold.Empty;
                  when others =>
                     Log.Error
                       ("Invalid setting value in " & Key & " = " & Value);
                     Success := False;
               end case;

            when "mold-alert" =>
               case Value is
                  when "NONE" | "None" | "none" =>
                     Settings.Alert := Mold.None;
                  when "WARNING" | "Warning" | "warning" =>
                     Settings.Alert := Mold.Warning;
                  when others =>
                     Log.Error
                       ("Invalid setting value in " & Key & " = " & Value);
                     Success := False;
               end case;

            when others =>
               Log.Error ("Invalid setting key in " & Key & " = " & Value);
               Success := False;
         end case;
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
              and then Element.Key.Slice (1, 5) = Mold.Variable_Setting_Prefix
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

            Inc (Results, Mold.Definitions);
         end loop;
      else
         Log.Debug ("Error reading definitions file");
      end if;

      Success := Read_Result.Success;
      return Vars;
   end Read_Variables;

end Definitions;
