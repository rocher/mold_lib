-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Simple_Logging.Decorators;

with Log_Exceptions; use Log_Exceptions;
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

   function Name return String is (Mold_Lib_Config.Crate_Name);

   -------------
   -- Version --
   -------------

   function Version return String is (Mold_Lib_Config.Crate_Version);

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
   )  return Natural
   --!pp on

   is
      Valid_Source_Path : Unbounded_String;
      Valid_Output_Path : Unbounded_String;
      Valid_Toml_Path   : Unbounded_String;

      Used_Settings : aliased Settings_Type :=
        (if Settings = null then Default_Settings else Settings.all);
   begin
      Log.Level := Log_Level;
      if Log.Level = Log.Debug then
         Log.Decorators.Location_Decorator :=
           Log.Decorators.Simple_Location_Decorator'Access;
      end if;

      if not Impl.Validation.Valid_Input_Paths
          (Source, Output_Dir, Toml_File, Valid_Source_Path, Valid_Output_Path,
           Valid_Toml_Path)
      then
         return 1;
      end if;

      declare
         Success     : Boolean;
         Errors      : Natural;
         Variables   : aliased Variables_Map;
         Source_Path : aliased String := To_String (Valid_Source_Path);
         Output_Path : aliased String := To_String (Valid_Output_Path);
         Toml_Path   : aliased constant String := To_String (Valid_Toml_Path);
      begin
         Impl.Args.Running_Directory :=
           To_Unbounded_String (Dir.Current_Directory);
         Impl.Args.Settings          := Used_Settings'Unrestricted_Access;
         Impl.Args.Variables         := Variables'Unchecked_Access;
         Impl.Args.Results           := Results;
         if Impl.Args.Results /= null then
            Impl.Args.Results.all := [others => 0];
         end if;
         Text_Filters.Set_Custom_Text_Filters (Filters);

         Variables := Impl.Variables.Read (Toml_Path, Success);

         if Success then
            Log.Debug ("  Toml_Path loaded");
         else
            Log.Error ("Cannot load toml file " & Toml_File);
            return 1;
         end if;

         Log.Debug ("BEGIN Mold_Lib.Apply");
         Log.Debug ("  Source_Path  : " & Source_Path);
         Log.Debug ("  Output_Path  : " & Output_Path);
         Log.Debug ("  Toml_Path    : " & Toml_Path);
         Log.Debug ("  Variables    : " & Variables'Image);

         Log.Debug ("Global Settings:" & Args.Settings.all'Image);

         if Dir.Kind (Source_Path) = Dir.Ordinary_File then
            Errors :=
              Impl.File.Replace
                (Source_Path'Unrestricted_Access,
                 Output_Path'Unrestricted_Access);
         else
            Log.Debug
              ("  File.Set_Running_Directory " & Dir.Current_Directory);
            Errors :=
              Impl.Directory.Replace
                ("", Source_Path'Unrestricted_Access,
                 Output_Path'Unrestricted_Access);
         end if;

         Log.Debug ("END Mold_Lib.Apply");
         return Errors;

         pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
      exception
         when E : Dir.Name_Error =>  --  raised by Dir.Kind
            Log_Exception (E, "Invalid source file '" & Source & "'");
            return 1;
            pragma Annotate (Xcov, Exempt_Off);
      end;

      pragma Annotate (Xcov, Exempt_On, "Top level exception caught");
   exception
      when E : others =>
         Log_Exception (E);
         return 1;
         pragma Annotate (Xcov, Exempt_Off);
   end Apply;

end Mold_Lib;
