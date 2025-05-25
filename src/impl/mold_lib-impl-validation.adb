-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Log_Exceptions; use Log_Exceptions;
with Log_Wrapper; use Log_Wrapper;

package body Mold_Lib.Impl.Validation is

   use all type Dir.File_Kind;

   ---------------------
   -- Validate_Source --
   ---------------------

   function Validate_Source
     (Source : String; Error : in out Boolean) return String with
     Pre => (Error = False)
   is
   begin
      return Source_Path : constant String := Full_Path_Expanded (Source) do
         if Source_Path'Length = 0 or else not Dir.Exists (Source_Path)
           or else Dir.Kind (Source_Path) = Dir.Special_File
         then
            Log.Error ("Invalid file or directory '" & Source_Path & "'");
            Error := True;
            return;
         end if;

         --  Source is either a file or directory:
         if Dir.Kind (Source_Path) = Dir.Ordinary_File then
            if Dir.Extension (Source_Path) = Mold_File_Extension then
               Log_Debug ("  Valid Source_Path");
            else
               Log.Error
                 ("Source file with invalid extension '" & Source_Path & "'");
               Error := True;
               return;
            end if;
         end if;
      end return;

      pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
   exception
      when E : Dir.Name_Error | Dir.Use_Error =>
         Log_Exception (E);
         Error := True;
         return "";
         pragma Annotate (Xcov, Exempt_Off);
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
            Log_Debug ("  Valid Output_Path");
         else
            Log_Debug ("Create output path " & Output_Dir_Path);
            Dir.Create_Path (Output_Dir_Path);
         end if;
      end return;

   exception
      when E : Dir.Name_Error | Dir.Use_Error =>
         Log_Exception (E);
         Error := True;
         return "";
   end Validate_Output_Dir;

   ------------------------
   -- Validate_Toml_File --
   ------------------------

   function Validate_Toml_File
     (Toml_File : String; Error : in out Boolean) return String
   is
   begin
      if Error then
         return "";
      end if;

      return Toml_Path : constant String := Full_Path_Expanded (Toml_File) do
         if Dir.Exists (Toml_Path)
           and then Dir.Kind (Toml_Path) = Dir.Ordinary_File
         then
            Log_Debug ("  Valid Toml_Path");
         else
            Log.Error ("Toml file not found '" & Toml_Path & "'");
            Error := True;
            return;
         end if;
      end return;

      pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
   exception
      when E : Dir.Name_Error | Dir.Use_Error =>
         Log_Exception (E);
         Error := True;
         return "";
         pragma Annotate (Xcov, Exempt_Off);

   end Validate_Toml_File;

   -----------------------
   -- Valid_Input_Paths --
   -----------------------

   --!pp off
   function Valid_Input_Paths (
      Source      :     String;
      Output_Dir  :     String;
      Toml_File   :     String;
      Source_Path : out Unbounded_String;
      Output_Path : out Unbounded_String;
      Toml_Path   : out Unbounded_String
   )  return Boolean
   --!pp on

   is
      Error : Boolean := False;

      Valid_Source_Path : constant String := Validate_Source (Source, Error);
      Valid_Output_Path : constant String :=
        Validate_Output_Dir (Valid_Source_Path, Output_Dir, Error);
      Valid_Toml_Path   : constant String :=
        Validate_Toml_File (Toml_File, Error);
   begin
      if not Error then
         Source_Path := To_Unbounded_String (Valid_Source_Path);
         Output_Path := To_Unbounded_String (Valid_Output_Path);
         Toml_Path   := To_Unbounded_String (Valid_Toml_Path);
      end if;
      return not Error;
   end Valid_Input_Paths;

end Mold_Lib.Impl.Validation;
