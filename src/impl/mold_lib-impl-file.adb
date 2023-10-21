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

   --------------------------
   -- Replace_In_File_Name --
   --------------------------

   function Replace_In_File_Name (Name : String) return String is
      Matches     : Reg.Match_Array (0 .. 3);
      New_Name    : Unbounded_String := To_Unbounded_String ("");
      Current     : Natural          := Name'First;
      Has_Matches : Boolean          := False;
   begin
      Log.Debug ("BEGIN Impl.File.Replace_In_File_Name");

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
               Inc_Result (Replacement_Warnings);
            else
               New_Name.Append (Var_Value);
            end if;
         end;

         Current := Matches (0).Last + 1;
      end loop;

      if Has_Matches then
         New_Name.Append (Name (Current .. Name'Last));
         Log.Debug ("  Renamed file " & Name & " to " & To_String (New_Name));
         Log.Debug ("END Replace_In_File_Name");
         return To_String (New_Name);
      else
         Log.Debug ("  No replacement done");
         Log.Debug ("END Replace_In_File_Name");
         return Name;
      end if;
   end Replace_In_File_Name;

   ------------------
   -- Include_Path --
   ------------------

   function Include_Path
     (File_Name : String; Success : out Boolean) return String
   is
      Extension : constant String := Dir.Extension (File_Name);
   begin
      Log.Debug ("BEGIN Impl.File.Include_Path");
      Log.Debug ("  File_Name : " & File_Name);
      Log.Debug ("  Extension : " & Extension);

      --  check extension file to be 'molt'
      --  *TODO - Consider relaxing this, with a flag or permanently

      if Extension /= Include_File_Extension then
         Success := False;
         Log.Error ("Invalid extension of include file " & File_Name);
         return "";
      end if;

      --  check whether File_Name exists or not: it can be a relative path to
      --  the currently processed file (e.g. lib/templates/foo.molt), or an
      --  absolute path to a library or repository of templates (e.g.
      --  /usr/share/molt/bar.molt)

      if Dir.Exists (File_Name)
        and then Dir.Kind (File_Name) = Dir.Ordinary_File
      then
         Success := True;
         Log.Debug ("  Including file " & File_Name);
         Log.Debug ("END Include_Path");
         return File_Name;
      end if;

      Log.Debug ("  Trying to include from running directory");

      declare
         File_Path : constant String :=
           Full_Path_Expanded (To_String (Args.Running_Directory), File_Name);
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
         Log_Exception (E, "Invalid include filename");
         return "";
         pragma Annotate (Xcov, Exempt_Off);

   end Include_Path;

   -----------------------
   -- Replace_In_Stream --
   -----------------------

   --!pp off
   procedure Replace_In_Stream
   (
      Input : in out Ada.Text_IO.File_Type;
      Output : Ada.Text_IO.File_Type
   )
   with
     Pre  => (Input.Is_Open and then Output.Is_Open),
     Post => (not Input.Is_Open and then Output.Is_Open)
   --!pp on
   is
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
                    Impl.Line.Replace (Line, Line_Number, Output);
               begin
                  if Args.Errors > 0 and then Args.Settings.Abort_On_Error then
                     goto Exit_Procedure;
                  end if;
                  Output.Put_Line (New_Line);
               end;
            else
               --  file inclusion
               declare
                  use Ada.Text_IO;

                  Inc_Name : constant String :=
                    Line (Matches (1).First .. Matches (1).Last);

                  Is_Valid : Boolean;
                  Inc_Path : constant String :=
                    Include_Path (Inc_Name, Is_Valid);
                  Inc_File : File_Type;
               begin

                  if not Is_Valid then
                     Log.Error ("Cannot find include file '" & Inc_Name & "'");
                     Args.Errors := @ + 1;
                     goto Exit_Procedure;
                  end if;

                  if Args.Included_Files.Contains
                      (To_Unbounded_String (Inc_Path))
                  then
                     Log.Error ("Circular inclusion of file " & Inc_Path);
                     Args.Errors := @ + 1;
                     goto Exit_Procedure;
                  else
                     Log.Debug ("  Including file " & Inc_Path & " ...");
                  end if;

                  Args.Included_Files.Append (To_Unbounded_String (Inc_Path));

                  Inc_File.Open (In_File, Inc_Path);
                  Replace_In_Stream (Inc_File, Output);

                  Args.Included_Files.Delete_Last;
                  Log.Debug ("  ...  file included");
               end;
            end if;
         end;
      end loop For_Each_Line;

      <<Exit_Procedure>>
      Input.Close;
      Log.Debug ("END File.Replace_In_Stream");

   end Replace_In_Stream;

   -------------
   -- Replace --
   -------------

   function Replace
     (Source, Output_Dir : not null String_Access) return Natural
   is
   begin
      Args.Source         := Source;
      Args.Errors         := 0;
      Args.Included_Files := Inclusion_Package.Empty_List;

      declare
         --  path to the source file
         Dir_Name : constant String := Dir.Containing_Directory (Source.all);

         --  base file name: no path & no mold extension
         Base_File_Name : constant String := Dir.Base_Name (Source.all);

         --  "preparation" file name: source dir + base file name
         Prep_File_Name : constant String :=
           Dir.Compose (Dir_Name, Base_File_Name);

         --  "Replaced" file name: variable substitution in "preparation" file
         --  name, if enabled
         Repl_File_Name : constant String :=
           (if Args.Settings.Replacement_In_File_Names then
              Replace_In_File_Name (Prep_File_Name)
            else Prep_File_Name);

         --  real output directory: the Output_Dir, if different from "", or
         --  the result directory after variable substitution, if enabled, in
         --  the path of the source file name
         Real_Out_Dir : constant String :=
           (if Output_Dir.all'Length > 0 then Output_Dir.all
            else Dir.Containing_Directory (Repl_File_Name));

         --  destination file name: composition of the real output dir and the
         --  source file name after variable substitution, if enabled
         Dst_File_Name : constant String :=
           Dir.Compose (Real_Out_Dir, Dir.Simple_Name (Repl_File_Name));

         Src_File : IO.File_Type;
         Dst_File : IO.File_Type;

      begin
         Log.Debug ("BEGIN Impl.File.Replace");
         Log.Debug ("  Dir_Name       : " & Dir_Name);
         Log.Debug ("  Src_File_Name  : " & Source.all);
         Log.Debug ("  Base_File_Name : " & Base_File_Name);
         Log.Debug ("  Prep_File_Name : " & Prep_File_Name);
         Log.Debug ("  Repl_File_Name : " & Repl_File_Name);
         Log.Debug ("  Real_Out_Dir   : " & Real_Out_Dir);
         Log.Debug ("  Dst_File_Name  : " & Dst_File_Name);

         Inc_Result (Files_Processed);

         if Base_File_Name /= Dir.Simple_Name (Dst_File_Name) then
            --  file name has variables successfully replaced
            Inc_Result (Files_Renamed);
         end if;

         --  open source file

         Src_File.Open (IO.In_File, Source.all);

         --  open or create destination file and directory

         if not Dir.Exists (Real_Out_Dir) then
            Dir.Create_Path (Real_Out_Dir);
            Log.Debug ("  Created dir " & Real_Out_Dir);
         end if;

         if Dir.Exists (Dst_File_Name) then
            if Args.Settings.Overwrite_Destination_Files then
               Dir.Delete_File (Dst_File_Name);
               Log.Debug ("  Deleted file " & Dst_File_Name);
               Inc_Result (Files_Overwritten);
            else
               Log.Error ("File " & Dst_File_Name & " already exists");
               Args.Errors := @ + 1;
               goto Exit_Function;
            end if;
         end if;

         Dst_File.Create (Name => Dst_File_Name);
         Log.Debug ("  Created file " & Dst_File_Name);

         --  perform variable substitution from Src to Dst
         Replace_In_Stream (Src_File, Dst_File);

         --  close Src file, and delete it when specified
         Dst_File.Close;
         if Args.Settings.Delete_Source_Files and then Args.Errors = 0 then
            Dir.Delete_File (Source.all);
            Log.Debug ("  Deleted file " & Source.all);
         end if;

         <<Exit_Function>>
         Args.Included_Files.Clear;
         Log.Debug ("END Impl.File.Replace");
         return Args.Errors;

         pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
      exception
         when E : Dir.Name_Error | Dir.Use_Error =>
            Log_Exception (E, "Invalid file or directory");
            Args.Errors := @ + 1;
            return Args.Errors;
            pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");

      end;
   end Replace;

end Mold_Lib.Impl.File;
