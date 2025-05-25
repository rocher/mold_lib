-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Simple_Logging.Decorators;

with Log_Exceptions; use Log_Exceptions;
with Log_Wrapper; use Log_Wrapper;
with Mold_Lib.Impl;  use Mold_Lib.Impl;
with Mold_Lib.Impl.Directory;
with Mold_Lib.Impl.File;
with Mold_Lib.Impl.Validation;
with Mold_Lib.Impl.Variables;
with Mold_Lib_Config;
with Text_Filters;

package body Mold_Lib is

   use all type Dir.File_Kind;
   use all type Log.Levels;

   ----------
   -- Name --
   ----------

   function Name return String
   is (Mold_Lib_Config.Crate_Name);

   -------------
   -- Version --
   -------------

   function Version return String
   is (Mold_Lib_Config.Crate_Version);

   --------------------
   -- Show_Variables --
   --------------------

   --!pp off
   function Show_Variables (
      Toml_File : String          := "mold.toml";
      Settings  : Settings_Access := null;
      Filters   : Filters_Access  := null;
      Results   : Results_Access  := null;
      Log_Level : Log.Levels      := Log.Info
   ) return Boolean
   --!pp on
   is
      Local_Settings : aliased Settings_Type :=
        (if Settings = null then Default_Settings else Settings.all);
   begin
      Local_Settings.Show_Variables := True;
      return
        Apply
          (Source     => ".",
           Output_Dir => "",
           Toml_File  => Toml_File,
           Settings   => Local_Settings'Unrestricted_Access,
           Filters    => Filters,
           Results    => Results,
           Log_Level  => Log_Level);
   end Show_Variables;

   -----------
   -- Apply --
   -----------

   --!pp off
   function Apply (
      Source     : String          := ".";
      Output_Dir : String          := "";
      Toml_File  : String          := "mold.toml";
      Settings   : Settings_Access := null;
      Filters    : Filters_Access  := null;
      Results    : Results_Access  := null;
      Log_Level  : Log.Levels      := Log.Info
   )  return Boolean
   --!pp on

   is
      Valid_Source_Path : Unbounded_String;
      Valid_Output_Path : Unbounded_String;
      Valid_Toml_Path   : Unbounded_String;

      Used_Settings : aliased Settings_Type :=
        (if Settings = null then Default_Settings else Settings.all);
   begin
      Log.Level := Log_Level;
      if Log.Level = Simple_Logging.Debug then
         Log.Decorators.Location_Decorator :=
           Log.Decorators.Simple_Location_Decorator'Access;
      end if;

      if not Impl.Validation.Valid_Input_Paths
               (Source,
                Output_Dir,
                Toml_File,
                Valid_Source_Path,
                Valid_Output_Path,
                Valid_Toml_Path)
      then
         return False;
      end if;

      declare
         Success     : Boolean;
         Variables   : aliased Variables_Map;
         Source_Path : aliased String := To_String (Valid_Source_Path);
         Output_Path : aliased String := To_String (Valid_Output_Path);
         Toml_Path   : aliased constant String := To_String (Valid_Toml_Path);
      begin
         Impl.Args.Running_Directory :=
           To_Unbounded_String (Dir.Current_Directory);
         Impl.Args.Settings := Used_Settings'Unrestricted_Access;
         Impl.Args.Variables := Variables'Unchecked_Access;
         Impl.Args.Results := Results;
         if Impl.Args.Results /= null then
            Impl.Args.Results.all := [others => 0];
         end if;
         Text_Filters.Set_Custom_Text_Filters (Filters);

         Variables := Impl.Variables.Read (Toml_Path, Success);
         if Success then
            Log_Debug ("  Toml_Path loaded");
         else
            Log.Error ("Cannot load toml file " & Toml_File);
            return False;
         end if;

         Success := Impl.Variables.Apply_Variable_Substitution (Variables);
         if Success then
            Log_Debug ("  Variable substitution applied to variables");
         else
            Log.Error ("Error applying variables substitution to variables");
            return False;
         end if;

         Log_Debug ("BEGIN Mold_Lib.Apply");
         Log_Debug ("  Source_Path  : " & Source_Path);
         Log_Debug ("  Output_Path  : " & Output_Path);
         Log_Debug ("  Toml_Path    : " & Toml_Path);
         Log_Debug ("  Variables    : " & Variables'Image);

         Log_Debug ("Global Settings:" & Args.Settings.all'Image);

         if Settings.Show_Variables then
            --  Show all variables defined in the toml file after value
            --  replacement and filters have been applied
            Impl.Variables.Show (Variables);
         else
            --  Proceed with the file or directories processing
            if Dir.Kind (Source_Path) = Dir.Ordinary_File then
               Success :=
                 Impl.File.Replace
                   (Source_Path'Unrestricted_Access,
                    Output_Path'Unrestricted_Access);
            else
               Log_Debug
                 ("  File.Set_Running_Directory " & Dir.Current_Directory);
               Success :=
                 Impl.Directory.Replace
                   ("",
                    Source_Path'Unrestricted_Access,
                    Output_Path'Unrestricted_Access);
            end if;
         end if;

         Log_Debug ("END Mold_Lib.Apply");
         return Success;

         pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
      exception
         when E : Dir.Name_Error =>
            --  raised by Dir.Kind
            Log_Exception (E, "Invalid source file '" & Source & "'");
            return False;
            pragma Annotate (Xcov, Exempt_Off);
      end;

      pragma Annotate (Xcov, Exempt_On, "Top level exception caught");
   exception
      when E : others =>
         Log_Exception (E);
         return False;
         pragma Annotate (Xcov, Exempt_Off);
   end Apply;

end Mold_Lib;
