-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.Regpat;
with Simple_Logging;

with Results; use Results;

package body File is

   package Dir renames Ada.Directories;
   package IO renames Ada.Text_IO;
   package Log renames Simple_Logging;
   package Reg renames GNAT.Regpat;

   package Inclusion_Package is new Doubly_Linked_Lists
     (Unbounded_String, "=");
   use all type Inclusion_Package.List;
   subtype Inclusion_List is Inclusion_Package.List;

   use all type Mold.Undef_Var_Action;
   use all type Mold.Undef_Var_Alert;
   use all type Reg.Match_Location;

   Var_Matcher     : Reg.Pattern_Matcher (256);
   File_Matcher    : Reg.Pattern_Matcher (256);
   Include_Matcher : Reg.Pattern_Matcher (128);

   type Global_Arguments is record
      Source         : String_Access;
      Variables      : Standard.Replace.Variables_Access;
      Settings       : Mold.Settings_Access;
      Results        : Mold.Results_Access;
      Errors         : Natural;
      Included_Files : Inclusion_List;
   end record;

   Global : Global_Arguments;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Var_Name : String) return String is
      use Standard.Replace.Variables_Package;
      Ref : constant Cursor :=
        Global.Variables.Find (To_Unbounded_String (Var_Name));
   begin
      if Ref = No_Element then
         Log.Debug ("Unmapped variable " & Var_Name);
         return "";
      else
         return To_String (Element (Ref));
      end if;
   end Get_Value;

   --------------------------
   -- Replace_In_File_Name --
   --------------------------

   function Replace_In_File_Name (Name : String) return String is
      Matches     : Reg.Match_Array (0 .. 3);
      New_Name    : Unbounded_String := To_Unbounded_String ("");
      Current     : Natural          := Name'First;
      Has_Matches : Boolean          := False;
   begin

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

            Var_Value : constant String := Get_Value (Var_Name);

            Is_Undefined : constant Boolean := (Var_Value = "");
         begin
            Log.Debug ("Pre_Name : '" & Pre_Name & "'");
            Log.Debug ("Var_Mold : '" & Var_Mold & "'");
            Log.Debug ("Var_Name : '" & Var_Name & "'");
            Log.Debug ("Var_Value: '" & Var_Value & "'");

            New_Name.Append (Pre_Name);
            if Is_Undefined then
               New_Name.Append (Var_Mold);
               Log.Warning
                 ("Undefined variable " & Var_Name &
                  " in file name substitution");
               Inc (Global.Results, Mold.Warnings);
            else
               New_Name.Append (Var_Value);
            end if;
         end;

         Current := Matches (0).Last + 1;
      end loop;

      if Has_Matches then
         New_Name.Append (Name (Current .. Name'Last));
         Log.Debug ("Renamed file " & Name & " to " & To_String (New_Name));
         return To_String (New_Name);
      else
         return Name;
      end if;
   end Replace_In_File_Name;

   ---------------------
   -- Replace_In_Line --
   ---------------------

   function Replace_In_Line (Line : String; Number : Natural) return String is
      Matches     : Reg.Match_Array (0 .. 3);
      New_Line    : Unbounded_String := To_Unbounded_String ("");
      Current     : Natural          := Line'First;
      Has_Matches : Boolean          := False;
   begin

      loop
         Var_Matcher.Match (Line, Matches, Current);
         exit when Matches (0) = Reg.No_Match;

         Has_Matches := True;
         Inc (Global.Results, Mold.Variables);

         declare
            Pre_Text : constant String :=
              Line (Matches (1).First .. Matches (1).Last);

            Var_Mold : constant String :=
              Line (Matches (2).First .. Matches (2).Last);

            Var_All_Name : constant String :=
              Line (Matches (3).First .. Matches (3).Last);

            Is_Mandatory : constant Boolean :=
              (Var_All_Name (Var_All_Name'First) =
               Mold.Mandatory_Substitution_Prefix);

            Is_Optional : constant Boolean :=
              (Var_All_Name (Var_All_Name'First) =
               Mold.Optional_Substitution_Prefix);

            Var_Name : constant String :=
              (if Is_Mandatory or Is_Optional then
                 Var_All_Name (Var_All_Name'First + 1 .. Var_All_Name'Last)
               else Var_All_Name);

            Var_Value : constant String := Get_Value (Var_Name);

            Is_Undefined : constant Boolean := (Var_Value = "");
         begin
            Log.Debug ("Pre_Text: '" & Pre_Text & "'");
            Log.Debug ("Var_Mold: '" & Var_Mold & "'");
            Log.Debug
              ("Var_All_Name: '" & Var_All_Name & "'" & " Fisrt=" &
               Var_All_Name'First'Image);
            Log.Debug ("Var_Name: '" & Var_Name & "'");
            New_Line.Append (Pre_Text);

            if Is_Undefined then
               Inc (Global.Results, Mold.Undefined);
               declare
                  LIN     : constant String := Number'Image;
                  COL     : constant String := Matches (2).First'Image;
                  Message : constant String :=
                    "Undefined variable '" & Var_Name & "' in " &
                    Global.Source.all & ":" & LIN (2 .. LIN'Last) & ":" &
                    COL (2 .. COL'Last);
               begin
                  if Is_Mandatory then
                     Inc (Global.Results, Mold.Ignored);
                     Inc (Global.Results, Mold.Errors);
                     New_Line.Append (Var_Mold);
                     Log.Error (Message);
                     Global.Errors := @ + 1;
                  elsif Is_Optional then
                     Inc (Global.Results, Mold.Emptied);
                  else  --  Is Normal
                     if Global.Settings.Alert = Mold.Warning then
                        Inc (Global.Results, Mold.Warnings);
                        Log.Warning (Message);
                     end if;
                     if Global.Settings.Action = Mold.Ignore then
                        Inc (Global.Results, Mold.Ignored);
                        New_Line.Append (Var_Mold);
                     else
                        Inc (Global.Results, Mold.Emptied);
                     end if;
                  end if;
               end;
            else
               Inc (Global.Results, Mold.Replaced);
               New_Line.Append (Var_Value);
            end if;
         end;

         Current := Matches (0).Last + 1;
      end loop;

      if Has_Matches then
         New_Line.Append (Line (Current .. Line'Last));
         return To_String (New_Line);
      else
         return Line;
      end if;
   end Replace_In_Line;

   -----------------------
   -- Replace_In_Stream --
   -----------------------

   procedure Replace_In_Stream
     (Src : in out Ada.Text_IO.File_Type; Dst : Ada.Text_IO.File_Type) with
     Pre  => (Src.Is_Open and then Dst.Is_Open),
     Post => (not Src.Is_Open and then Dst.Is_Open)
   is
      Line_Number : Natural := 0;
      Matches     : Reg.Match_Array (0 .. 1);
   begin
      For_Each_Line :
      loop
         exit For_Each_Line when Src.End_Of_File;
         Line_Number := @ + 1;
         declare
            Line : constant String := Src.Get_Line;
         begin
            Include_Matcher.Match (Line, Matches);
            if Matches (0) = Reg.No_Match then
               --  variable substitution
               declare
                  New_Line : constant String :=
                    Replace_In_Line (Line, Line_Number);
               begin
                  if Global.Errors > 0 and then Global.Settings.Abort_On_Error
                  then
                     goto Exit_Procedure;
                  end if;
                  Dst.Put_Line (New_Line);
               end;
            else
               --  file inclusion
               declare
                  Inc_Name  : constant String :=
                    Line (Matches (1).First .. Matches (1).Last);
                  Full_Name : constant String := Dir.Full_Name (Inc_Name);
                  Extension : constant String := Dir.Extension (Inc_Name);
               begin
                  Log.Debug ("Include file " & Full_Name);

                  if Extension /= Mold.Include_File_Extension then
                     Log.Error
                       ("Invalid extension of include file " & Inc_Name);
                     Global.Errors := @ + 1;
                     goto Exit_Procedure;
                  end if;

                  if not Dir.Exists (Full_Name) then
                     Log.Error ("Include file not found " & Full_Name);
                     Global.Errors := @ + 1;
                     goto Exit_Procedure;
                  end if;

                  if Global.Included_Files.Contains
                      (To_Unbounded_String (Full_Name))
                  then
                     Log.Error ("Circular inclusion of file " & Full_Name);
                     Global.Errors := @ + 1;
                     goto Exit_Procedure;
                  end if;

                  declare
                     use Ada.Text_IO;
                     Inc : File_Type;
                  begin
                     Global.Included_Files.Append
                       (To_Unbounded_String (Full_Name));
                     Inc.Open (In_File, Full_Name);
                     Replace_In_Stream (Inc, Dst);
                     Global.Included_Files.Delete_Last;
                  end;
               end;
            end if;
         end;
      end loop For_Each_Line;

      <<Exit_Procedure>>
      Src.Close;
   end Replace_In_Stream;

   -------------
   -- Replace --
   -------------

   --!pp off
   function Replace
   (
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Standard.Replace.Variables_Access;
      Settings   : not null Mold.Settings_Access;
      Results    :          Mold.Results_Access := null
   )
   return Natural
   --!pp on

   is
   begin
      --!pp off
      Global := (Source         => Source,
                 Variables      => Variables,
                 Settings       => Settings,
                 Results        => Results,
                 Errors         => 0,
                 Included_Files => Inclusion_Package.Empty_List);
      --!pp on

      declare
         --  path to the source file
         Dir_Name : constant String := Dir.Containing_Directory (Source.all);

         --  base file name: no path & no mold extension
         Base_File_Name : constant String := Dir.Base_Name (Source.all);

         --  "preparation" file name: source dir + base file name
         Prep_File_Name : constant String :=
           Dir.Compose (Dir_Name, Base_File_Name);

         --  "replaced" file name: variable substitution in "preparation"
         --  file name, if enabled
         Repl_File_Name : constant String :=
           (if Settings.Rename_Source then
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
         if Global.Errors > 0 then
            --  error detected during file name substitution, in the function
            --  Replace_In_File_Name
            return Global.Errors;
         end if;

         Inc (Results, Mold.Files);

         if Base_File_Name /= Dir.Simple_Name (Dst_File_Name) then
            --  file name has variables successfully replaced
            Inc (Results, Mold.Renamed);
         end if;

         Log.Debug ("Dir_Name       : " & Dir_Name);
         Log.Debug ("Src_File_Name  : " & Source.all);
         Log.Debug ("Base_File_Name : " & Base_File_Name);
         Log.Debug ("Prep_File_Name : " & Prep_File_Name);
         Log.Debug ("Repl_File_Name : " & Repl_File_Name);
         Log.Debug ("Real_Out_Dir   : " & Real_Out_Dir);
         Log.Debug ("Dst_File_Name  : " & Dst_File_Name);

         --  open source file
         Src_File.Open (IO.In_File, Source.all);

         --  open destination file
         if Dir.Exists (Dst_File_Name) then
            if Settings.Overwrite then
               Dir.Delete_File (Dst_File_Name);
               Log.Debug ("Deleted file " & Dst_File_Name);
               Inc (Results, Mold.Overwritten);
            else
               Log.Error ("File " & Dst_File_Name & " already exists");
               Global.Errors := @ + 1;
               return Global.Errors;
            end if;
         end if;
         Dst_File.Create (Name => Dst_File_Name);
         Log.Debug ("Created file " & Dst_File_Name);

         Replace_In_Stream (Src_File, Dst_File);

         Dst_File.Close;
         if Settings.Delete_Source and then Global.Errors = 0 then
            Dir.Delete_File (Source.all);
         end if;

         Global.Included_Files.Clear;
         return Global.Errors;

      exception
         --  file name with replaced variables yields an invalid file name
         when Dir.Name_Error =>
            Log.Error
              ("Invalid replacement in file name: '" & Dst_File_Name & "'");
            Global.Errors := @ + 1;
            return Global.Errors;
      end;
   end Replace;

begin

   --                              .------.
   --                              |   3  |
   Var_Matcher.Compile ("(.*?)({{ *([^} ]+) *}})");
   --                    | 1 ||        2       |
   --                    '---''----------------'
   --  Example:
   --
   --              1         2         3
   --     1234567890123456789012345678901234567
   --     This is a {{ #foo }} variable example
   --
   --                     Matches (0) = ( 1, 20) = "This is a {{ #foo }}"
   --     Pre_Text     := Matches (1) = ( 1, 10) = "This is a "
   --     Var_Mold     := Matches (2) = (11, 20) =            "{{ #foo }}"
   --     Var_All_Name := Matches (3) = (14, 17) =               "#foo"
   --     Var_Name     := ( remove # if exists ) =                "foo"
   --  ------------------------------------------------------------------------

   --
   --                             .------.
   --                             |   3  |
   File_Matcher.Compile ("(.*?)(__([^_]+?)__)");
   --                     | 1 ||     2      |
   --                     '---''------------'
   --  Example:
   --
   --              1         2
   --     123456789012345678901
   --     README-__PURPOSE__.md
   --
   --                     Matches (0) = ( 1, 18) = "README-__PURPOSE__"
   --     Pre_Text     := Matches (1) = ( 1,  7) = "README-"
   --     Var_Mold     := Matches (2) = ( 8, 18) =         "__PURPOSE__"
   --     Var_All_Name := Matches (3) = (10, 16) =           "PURPOSE"
   --  ------------------------------------------------------------------------

   --
   Include_Matcher.Compile
     ("^{{ *" & Mold.Inclusion_File_Prefix & "([^ ]+) *}}$");
   --                                         |  1  |
   --                                         '-----'
   --  Example:
   --
   --              1         2
   --     1234567890123456789012
   --     {{ include:foo.molt }}
   --
   --                     Matches (0) = ( 1, 22) = "{{ include:foo.molt }}"
   --     Var_Name     := Matches (1) = (12, 19) =            "foo.molt"
   --  ------------------------------------------------------------------------

end File;
