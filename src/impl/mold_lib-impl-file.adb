-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Log_Exceptions; use Log_Exceptions;
with Mold_Lib.Impl.Line;
with Mold_Lib.Impl.Variables;

package body Mold_Lib.Impl.File is

   use all type Dir.File_Kind;
   use all type Reg.Match_Location;

   -------------------------
   -- Replace_In_Filename --
   -------------------------

   function Replace_In_Filename (Name : String) return String is
      Matches     : Reg.Match_Array (0 .. 3);
      New_Name    : Unbounded_String := To_Unbounded_String ("");
      Current     : Natural          := Name'First;
      Has_Matches : Boolean          := False;
   begin
      Log.Debug ("BEGIN Impl.File.Replace_In_Filename");

      loop
         File_Matcher.Match (Name, Matches, Current);
         exit when Matches (0) = Reg.No_Match;

         Has_Matches := True;
         declare
            Pre_Name : constant String :=
              Name (Matches (1).First .. Matches (1).Last);

            Var_Mold : constant String :=
              Name (Matches (2).First .. Matches (2).Last);

            Var_Name : constant String :=
              Name (Matches (3).First .. Matches (3).Last);

            Var_Value : constant String := Impl.Variables.Get_Value (Var_Name);

            Is_Undefined : constant Boolean := (Var_Value = "");
         begin

            Log.Debug ("  Pre_Name : '" & Pre_Name & "'");
            Log.Debug ("  Var_Mold : '" & Var_Mold & "'");
            Log.Debug ("  Var_Name : '" & Var_Name & "'");
            Log.Debug ("  Var_Value: '" & Var_Value & "'");

            New_Name.Append (Pre_Name);
            if Is_Undefined then
               New_Name.Append (Var_Mold);
               Log.Warning
                 ("  Undefined variable " & Var_Name &
                  " in file name substitution");
               Inc_Result (Warnings);
            else
               New_Name.Append (Var_Value);
            end if;
         end;

         Current := Matches (0).Last + 1;
      end loop;

      if Has_Matches then
         New_Name.Append (Name (Current .. Name'Last));
         Log.Debug ("  Renamed file " & Name & " to " & To_String (New_Name));
         Log.Debug ("END Replace_In_Filename");
         return To_String (New_Name);
      else
         Log.Debug ("  No replacement done");
         Log.Debug ("END Replace_In_Filename");
         return Name;
      end if;
   end Replace_In_Filename;

   ------------------
   -- Include_Path --
   ------------------

   function Include_Path
     (Filename : String; Success : in out Boolean) return String
   is
      Extension : constant String := Dir.Extension (Filename);
   begin
      Log.Debug ("BEGIN Impl.File.Include_Path");
      Log.Debug ("  Filename  : " & Filename);
      Log.Debug ("  Extension : " & Extension);

      --  check extension file to be 'molt'
      --  *TODO - Consider relaxing this, with a flag or permanently

      if Extension /= Include_File_Extension then
         Success := False;
         Log.Error ("Invalid extension of include file " & Filename);
         return "";
      end if;

      --  check whether Filename exists or not: it can be a relative path to
      --  the currently processed file (e.g. lib/templates/foo.molt), or an
      --  absolute path to a library or repository of templates (e.g.
      --  /usr/share/molt/bar.molt)

      if Dir.Exists (Filename) and then Dir.Kind (Filename) = Dir.Ordinary_File
      then
         Success := True;
         Log.Debug ("  Including file " & Filename);
         Log.Debug ("END Include_Path");
         return Filename;
      end if;

      Log.Debug ("  Trying to include from running directory");

      declare
         File_Path : constant String :=
           Full_Path_Expanded (To_String (Args.Running_Directory), Filename);
      begin
         Log.Debug ("  File_Path : " & File_Path);

         if Dir.Exists (File_Path)
           and then Dir.Kind (File_Path) = Dir.Ordinary_File
         then
            Success := True;
            Log.Debug ("  Include from running directory " & File_Path);
            Log.Debug ("END Include_Path");
            return File_Path;
         else
            Success := False;
            Log.Debug ("  File not found " & File_Path);
            Log.Debug ("END Include_Path");
            return "";
         end if;
      end;

      pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
   exception
      when E : Dir.Name_Error | Dir.Use_Error =>
         Log_Exception (E);
         return "";
         pragma Annotate (Xcov, Exempt_Off);

   end Include_Path;

   -----------------------
   -- Replace_In_Stream --
   -----------------------

   --!pp off
   function Replace_In_Stream
   (
      Input : in out Ada.Text_IO.File_Type;
      Output : Ada.Text_IO.File_Type
   ) return Boolean
   with
     Pre  => (Input.Is_Open and then Output.Is_Open),
     Post => (not Input.Is_Open and then Output.Is_Open)
   --!pp on
   is
      Success     : Boolean;
      Line_Number : Natural := 0;
      Matches     : Reg.Match_Array (0 .. 1);
   begin
      Log.Debug ("BEGIN Impl.File.Replace_In_Stream");
      For_Each_Line :
      loop
         exit For_Each_Line when Input.End_Of_File;
         Line_Number := @ + 1;
         declare
            Line : constant String := Input.Get_Line;
         begin
            Include_Matcher.Match (Line, Matches);
            if Matches (0) = Reg.No_Match then
               --  variable substitution
               declare
                  New_Line : constant String :=
                    Impl.Line.Replace (Line, Line_Number, Output, Success);
               begin
                  if not Success then
                     --  error logged in Impl.Line.Replace
                     goto Exit_Function;
                  end if;
                  Output.Put_Line (New_Line);
               end;
            else
               --  file inclusion
               declare
                  use Ada.Text_IO;

                  Inc_Name : constant String :=
                    Line (Matches (1).First .. Matches (1).Last);

                  Inc_Path : constant String :=
                    Include_Path (Inc_Name, Success);
                  Inc_File : File_Type;
               begin

                  if not Success then
                     Log.Error ("Cannot find include file '" & Inc_Name & "'");
                     goto Exit_Function;
                  end if;

                  if Args.Included_Files.Contains
                      (To_Unbounded_String (Inc_Path))
                  then
                     Log.Error ("Circular inclusion of file " & Inc_Path);
                     Success := False;
                     goto Exit_Function;
                  else
                     Log.Detail ("including file " & Inc_Path);
                  end if;

                  Args.Included_Files.Append (To_Unbounded_String (Inc_Path));

                  Inc_File.Open (In_File, Inc_Path);
                  Success := Replace_In_Stream (Inc_File, Output);

                  Args.Included_Files.Delete_Last;
               end;
            end if;
         end;
      end loop For_Each_Line;

      <<Exit_Function>>
      Input.Close;
      Log.Debug ("END File.Replace_In_Stream");
      return Success;

   end Replace_In_Stream;

   -------------
   -- Replace --
   -------------

   function Replace
     (Source, Output_Dir : not null String_Access) return Boolean
   is
      Success : Boolean;
   begin
      Args.Source         := Source;
      Args.Included_Files := Inclusion_Package.Empty_List;

      declare
         --  path to the source file
         Dir_Name : constant String := Dir.Containing_Directory (Source.all);

         --  base file name: no path & no mold extension
         Base_Filename : constant String := Dir.Base_Name (Source.all);

         --  "preparation" file name: source dir + base file name
         Prep_Filename : constant String :=
           Dir.Compose (Dir_Name, Base_Filename);

         --  "Replaced" file name: variable substitution in "preparation" file
         --  name, if enabled
         Repl_Filename : constant String :=
           (if Args.Settings.Replacement_In_Filenames then
              Replace_In_Filename (Prep_Filename)
            else Prep_Filename);

         --  real output directory: the Output_Dir, if different from "", or
         --  the result directory after variable substitution, if enabled, in
         --  the path of the source file name
         Real_Out_Dir : constant String :=
           (if Output_Dir.all'Length > 0 then Output_Dir.all
            else Dir.Containing_Directory (Repl_Filename));

         --  destination file name: composition of the real output dir and the
         --  source file name after variable substitution, if enabled
         Dst_Filename : constant String :=
           Dir.Compose (Real_Out_Dir, Dir.Simple_Name (Repl_Filename));

         Src_File : IO.File_Type;
         Dst_File : IO.File_Type;

      begin
         Log.Debug ("BEGIN Impl.File.Replace");
         Log.Debug ("  Dir_Name      : " & Dir_Name);
         Log.Debug ("  Src_Filename  : " & Source.all);
         Log.Debug ("  Base_Filename : " & Base_Filename);
         Log.Debug ("  Prep_Filename : " & Prep_Filename);
         Log.Debug ("  Repl_Filename : " & Repl_Filename);
         Log.Debug ("  Real_Out_Dir  : " & Real_Out_Dir);
         Log.Debug ("  Dst_Filename  : " & Dst_Filename);

         Inc_Result (Files_Processed);

         if Base_Filename /= Dir.Simple_Name (Dst_Filename) then
            --  filename has variables successfully replaced
            Inc_Result (Files_Renamed);
            Log.Detail ("renamed destination file " & Dst_Filename);
         end if;

         --  open source file

         Src_File.Open (IO.In_File, Source.all);

         --  open or create destination file and directory

         if not Dir.Exists (Real_Out_Dir) then
            Dir.Create_Path (Real_Out_Dir);
            Log.Detail ("created directory " & Real_Out_Dir);
         end if;

         if Dir.Exists (Dst_Filename) then
            if Args.Settings.Overwrite_Destination_Files then
               Dir.Delete_File (Dst_Filename);
               Inc_Result (Files_Overwritten);
               Log.Detail ("deleted destination file " & Dst_Filename);
            else
               Log.Error
                 ("Destination file " & Dst_Filename & " already exists");
               Success := False;
               goto Exit_Function;
            end if;
         end if;

         Dst_File.Create (Name => Dst_Filename);
         Log.Detail ("created destination file " & Dst_Filename);

         --  perform variable substitution from Src to Dst
         Success := Replace_In_Stream (Src_File, Dst_File);

         --  close Src file, and delete it when specified
         Dst_File.Close;
         if Args.Settings.Delete_Source_Files and then Args.Errors = 0 then
            Dir.Delete_File (Source.all);
            Inc_Result (Files_Deleted);
            Log.Detail ("deleted source file " & Source.all);
         end if;

         <<Exit_Function>>
         Args.Included_Files.Clear;
         Log.Debug ("END Impl.File.Replace");
         return Success;

      exception
         when E : Dir.Name_Error | Dir.Use_Error =>
            Log_Exception (E);
            return False;
      end;
   end Replace;

end Mold_Lib.Impl.File;
