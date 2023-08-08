-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Definitions is

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
                     Success := False;
               end case;

            when others =>
               Success := False;
         end case;
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
   begin
      return Empty_Map;
   end Read_Variables;

end Definitions;
