-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with TOML;
with TOML.File_IO;

with Log_Exceptions; use Log_Exceptions;
with Mold_Lib.Impl.Text;

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
         if Value = "TRUE" or else Value = "True" or else Value = "true" then
            Variable.all := True;
         elsif Value = "FALSE" or else Value = "False" or else Value = "false"
         then
            Variable.all := False;
         else
            Log.Error ("Invalid setting value in " & Key & " = " & Value);
            Success := False;
         end if;
      end Set_Boolean;

   begin
      if Key = "mold-replacement-in-filenames" then
         Set_Boolean (Args.Settings.Replacement_In_Filenames'Access);

      elsif Key = "mold-replacement-in-variables" then
         Set_Boolean (Args.Settings.Replacement_In_Variables'Access);

      elsif Key = "mold-delete-source-files" then
         Set_Boolean (Args.Settings.Delete_Source_Files'Access);

      elsif Key = "mold-overwrite-destination-files" then
         Set_Boolean (Args.Settings.Overwrite_Destination_Files'Access);

      elsif Key = "mold-on-undefined" then
         begin
            Args.Settings.On_Undefined := On_Undefined_Handling'Value (Value);
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
         Log.Detail ("setting applied " & Key & " = " & Value);
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
              and then Element.Key.Length
                       >= 10 --  minimum length of defined settings
              and then Element.Key.Slice (1, Defined_Setting_Prefix'Length)
                       = Defined_Setting_Prefix
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
         Log.Error ("Cannot load variables file");
      end if;

      Success := Read_Result.Success;
      return Vars;

   exception
      when E : Constraint_Error =>
         Log_Exception (E);
         Success := False;
         return Empty_Map;

   end Read;

   ----------
   -- Show --
   ----------

   procedure Show (Variables : Variables_Map) is
      use all type Variables_Package.Cursor;

      Cursor : Variables_Package.Cursor := Variables.First;
   begin
      loop
         exit when Cursor = Variables_Package.No_Element;
         declare
            Var_Name : constant String := To_String (Cursor.Key);
            Value    : constant String := Get_Value (To_String (Cursor.Key));
         begin
            Ada.Text_IO.Put_Line
              ("Variable: " & Var_Name & " = '" & Value & "'");
         end;
         Cursor := Cursor.Next;
      end loop;
   end Show;

   ---------------------------------
   -- Apply_Variable_Substitution --
   ---------------------------------

   function Apply_Variable_Substitution
     (Variables : in out Variables_Map) return Boolean
   is
      use all type Variables_Package.Cursor;

      Loops       : Natural := 0;
      Cursor      : Variables_Package.Cursor;
      Has_Changes : Boolean;
   begin
      if Variables.Length = 0
        or else Args.Settings.Replacement_In_Variables = False
      then
         return True;
      end if;

      loop
         --  loop several times the variable substitution process to allow for
         --  nested variables (e.g. foo = {{bar}}, bar = {{baz}}, baz = "foo")
         Loops := Loops + 1;
         Cursor := Variables.First;
         Has_Changes := False;
         loop
            --  loop over all variables defined in the toml file
            Log.Debug ("Var subst BEGIN : " & To_String (Cursor.Key));
            declare
               Success   : Boolean;
               Var_Name  : constant String := To_String (Cursor.Key);
               Value     : constant String :=
                 Get_Value (To_String (Cursor.Key));
               New_Value : constant String :=
                 Impl.Text.Replace
                   (Value, Impl.Text.variable, Loops, Var_Name, Success);
            begin
               if not Success then
                  return False;
               end if;
               if Value /= New_Value then
                  Has_Changes := True;
                  Variables.Replace
                    (Cursor.Key, To_Unbounded_String (New_Value));
               end if;
               Log.Debug
                 ("Var subst END   : "
                  & Var_Name
                  & " --> '"
                  & Value
                  & "' --> '"
                  & New_Value
                  & "'");
            end;
            Cursor := Cursor.Next;
            exit when Cursor = Variables_Package.No_Element;
         end loop;

         exit when not Has_Changes or else Loops = 10;
         --  # TODO  --  Investigate the number of loops needed to converge
         --              depending on the number of variables defined.
         --
         --  It seems that 10 is a good number for most cases. The number of
         --  loops is limited to avoid infinite loops in case of circular
         --  references. The magic number 10 has been obtained experimentally
         --  with a cycle of 1000 variables.
      end loop;

      return True;
   end Apply_Variable_Substitution;

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
