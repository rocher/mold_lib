-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Simple_Logging.Decorators;

with Definitions;
with Directory;
with Dir_Ops; use Dir_Ops;
with File;

package body Mold_Lib is

   package Dir renames Ada.Directories;

   use all type Dir.File_Kind;
   use all type Log.Levels;

   ---------------------
   -- Validate_Source --
   ---------------------

   function Validate_Source
     (Source : String; Error : in out Boolean) return String
   is
   begin
      if Error then
         return "";
      end if;

      return Source_Path : constant String := Full_Path_Expanded (Source) do
         if Source_Path'Length = 0 or else not Dir.Exists (Source_Path)
           or else Dir.Kind (Source_Path) = Dir.Special_File
         then
            Log.Error ("Invalid file or directory '" & Source_Path & "'");
            Error := True;
            return;
         end if;

         --  Source is either a file or directory

         if Dir.Kind (Source_Path) = Dir.Ordinary_File then
            if Dir.Extension (Source_Path) = Mold_File_Extension then
               Log.Debug ("  Valid Source_Path");
            else
               Log.Error
                 ("Source file with invalid extension '" & Source_Path & "'");
               Error := True;
               return;
            end if;
         end if;
      end return;

   exception
      when Dir.Name_Error =>
         Log.Error ("Invalid source file or directory");
         Error := True;
         return "";
   end Validate_Source;

   -------------------------
   -- Validate_Output_Dir --
   -------------------------

   function Validate_Output_Dir
     (Source_Path, Output_Dir : String; Error : in out Boolean) return String
   is
   begin
      if Error then
         return "";
      end if;

      return
        Output_Dir_Path : constant String :=
          (if Output_Dir'Length > 0 then Full_Path_Expanded (Output_Dir)
           elsif Dir.Kind (Source_Path) = Dir.Ordinary_File then
             Dir.Containing_Directory (Source_Path)
           else Source_Path)
      do
         if Dir.Exists (Output_Dir_Path) then
            Log.Debug ("  Valid Output_Path");
         else
            Log.Debug ("Create output path " & Output_Dir_Path);
            Dir.Create_Path (Output_Dir_Path);
         end if;
      end return;

   exception
      when Dir.Name_Error | Dir.Use_Error =>
         Log.Error ("Invalid output directory");
         Error := True;
         return "";
   end Validate_Output_Dir;

   --------------------------
   -- Validate_Definitions --
   --------------------------

   function Validate_Definitions
     (Definitions : String; Error : in out Boolean) return String
   is
   begin
      if Error then
         return "";
      end if;

      return
        Definitions_Path : constant String := Full_Path_Expanded (Definitions)
      do
         if Dir.Exists (Definitions_Path)
           and then Dir.Kind (Definitions_Path) = Dir.Ordinary_File
         then
            Log.Debug ("  Valid Definitions_Path");
         else
            Log.Error
              ("Definitions file not found '" & Definitions_Path & "'");
            Error := True;
            return;
         end if;
      end return;

   exception
      when Dir.Name_Error =>
         Log.Error ("Invalid definitions file");
         Error := True;
         return "";
   end Validate_Definitions;

   -----------
   -- Apply --
   -----------

   --!pp off
   function Apply
   (
      Source      : String          := ".";
      Output_Dir  : String          := "";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := null;
      Results     : Results_Access  := null;
      Log_Level   : Log.Levels      := Log.Info
   )
   return Natural
   --!pp on

   is
      Validation_Error : Boolean         := False;
      Definitions_Path : constant String :=
        Validate_Definitions (Definitions, Validation_Error);
      Source_Path      : aliased String  :=
        Validate_Source (Source, Validation_Error);
      Output_Dir_Path  : aliased String  :=
        Validate_Output_Dir (Source_Path, Output_Dir, Validation_Error);

      Used_Settings : constant Settings_Access :=
        (if Settings = null then Default_Settings'Access else Settings);
   begin

      Log.Level := Log_Level;
      if Log.Level = Log.Debug then
         Log.Decorators.Location_Decorator :=
           Log.Decorators.Simple_Location_Decorator'Access;
      end if;

      if Results /= null then
         Results.all := [others => 0];
      end if;

      Log.Debug ("MOLD Apply");
      Log.Debug ("  Source_Path      : " & Source_Path);
      Log.Debug ("  Output_Dir_Path  : " & Output_Dir_Path);
      Log.Debug ("  Definitions_Path : " & Definitions_Path);

      if Validation_Error then
         return 1;
      end if;

      declare
         Variables : aliased Standard.Definitions.Variables_Map;
         Success   : Boolean;
         Errors    : Natural;
      begin
         Variables :=
           Standard.Definitions.Read_Variables
             (Definitions_Path, Used_Settings, Results, Success);

         if Success then
            Log.Debug ("  Definitions_Path loaded");
         else
            Log.Error ("Cannot load definitions file");
            return 1;
         end if;

         if Dir.Kind (Source_Path) = Dir.Ordinary_File then
            Errors :=
              File.Replace
                (Source_Path'Unrestricted_Access,
                 Output_Dir_Path'Unrestricted_Access,
                 Variables'Unchecked_Access, Used_Settings, Results);
         else
            Log.Debug
              ("  File.Set_Running_Directory " & Dir.Current_Directory);
            File.Set_Running_Directory (Dir.Current_Directory);
            Errors :=
              Directory.Replace
                ("", Source_Path'Unrestricted_Access,
                 Output_Dir_Path'Unrestricted_Access,
                 Variables'Unchecked_Access, Used_Settings, Results);
         end if;

         return Errors;
      end;

   exception
      when others =>
         Log.Error
           ("EXCEPTION caught in Mold.Apply:" &
            " Please run again with logging Debug enabled" &
            " and report this error");
         return 1;

   end Apply;

end Mold_Lib;
