-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.Regpat;
with Simple_Logging;

package body File is

   package Dir renames Ada.Directories;
   package IO renames Ada.Text_IO;
   package Log renames Simple_Logging;
   package Reg renames GNAT.Regpat;

   use all type Mold.Undef_Var_Action;
   use all type Mold.Undef_Var_Alert;
   use all type Reg.Match_Location;

   Var_Matcher  : Reg.Pattern_Matcher (256);
   File_Matcher : Reg.Pattern_Matcher (256);

   -------------
   -- Replace --
   -------------

   function Replace
   --!pp off
   (
      Name      : String;
      Variables : Subs.Variables_Access;
      Settings  : Mold.Settings_Access;
      Results   : Mold.Results_Access
   )
   --!pp on

      return Natural
   is
      Src_File      : IO.File_Type;
      Dst_File      : IO.File_Type;
      Dst_File_Name : constant String := Dir.Base_Name (Name);
      Replacements  : Natural         := 0;
      Line_Number   : Natural         := 0;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Var_Name : String) return String is
         use Subs.Variables_Package;
         Ref : constant Cursor :=
           Variables.Find (To_Unbounded_String (Var_Name));
      begin
         if Ref = No_Element then
            return "";
         else
            return To_String (Element (Ref));
         end if;
      end Get_Value;

      ---------------------
      -- Replace_In_Line --
      ---------------------

      function Replace_In_Line (Line : String) return String is
         Matches     : Reg.Match_Array (0 .. 3);
         New_Line    : Unbounded_String := To_Unbounded_String ("");
         Current     : Natural          := Line'First;
         Has_Matches : Boolean          := False;
      begin

         loop
            Var_Matcher.Match (Line, Matches, Current);
            exit when Matches (0) = Reg.No_Match;

            Has_Matches := True;
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

               Var_Name : constant String :=
                 (if Is_Mandatory then
                    Var_All_Name (Var_All_Name'First + 1 .. Var_All_Name'Last)
                  else Var_All_Name);

               Var_Value : constant String := Get_Value (Var_Name);

               Undefined_Var : constant Boolean := (Var_Value = "");
            begin
               Log.Debug ("Pre_Text: '" & Pre_Text & "'");
               Log.Debug ("Var_Mold: '" & Var_Mold & "'");
               Log.Debug
                 ("Var_All_Name: '" & Var_All_Name & "'" & " Fisrt=" &
                  Var_All_Name'First'Image);
               Log.Debug ("Var_Name: '" & Var_Name & "'");
               New_Line.Append (Pre_Text);
               if Undefined_Var then
                  declare
                     LIN     : constant String := Line_Number'Image;
                     COL     : constant String := Matches (2).First'Image;
                     Message : constant String :=
                       "Undefined variable '" & Var_Name & "' in " &
                       Dst_File_Name & ":" & LIN (2 .. LIN'Last) & ":" &
                       COL (2 .. COL'Last);
                  begin
                     if Is_Mandatory then
                        Log.Error (Message);
                        New_Line.Append (Var_Mold);
                     else
                        if Settings.Alert = Mold.Warning then
                           Log.Warning (Message);
                        end if;
                        if Settings.Action = Mold.Ignore then
                           New_Line.Append (Var_Mold);
                        end if;
                     end if;
                  end;
               else
                  New_Line.Append (Var_Value);
                  Replacements := @ + 1;
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

   begin
      if Dir.Exists (Dst_File_Name) then
         Dir.Delete_File (Dst_File_Name);
         --  Log.Error ("File " & Dst_File_Name & " already exists, skipped");
         --  return 0;
      end if;

      Src_File.Open (IO.In_File, Name);
      --  Dst_File.Open (IO.Out_File, Dst_File_Name);
      Dst_File.Create (Name => Dst_File_Name);

      loop
         exit when Src_File.End_Of_File;
         Line_Number := @ + 1;
         declare
            Line     : constant String := Src_File.Get_Line;
            New_Line : constant String := Replace_In_Line (Line);
         begin
            Dst_File.Put_Line (New_Line);
         end;
      end loop;

      Dst_File.Close;
      Src_File.Close;
      --  Dir.Delete_File (Name);

      return Replacements;
   end Replace;

begin

   --                             .-----.
   --                             |  3  |
   Var_Matcher.Compile ("(.*)({{ *([^}]+) *}})");
   --                    | 1||       2       |
   --                    '--''---------------'
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

   File_Matcher.Compile ("(.*)(__([^_]+)__)");

end File;
