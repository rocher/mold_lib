-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib) TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;

with GNAT.MD5;

with Simple_Logging;

package body Support is

   ------------------
   -- Pretty_Print --
   ------------------

   function Pretty_Print
     (Errors : Natural; Results : Mold.Results_Access) return String
   is
      Fill : constant String  := "           ";
      Text : Unbounded_String := To_Unbounded_String ("");
   begin
      Text.Append (ASCII.LF & "    Errors:" & Errors'Image);
      Text.Append (ASCII.LF & "    Results:" & ASCII.LF);

      for Field in Mold.Field_Type loop
         Text.Append ("       " & Field'Image);
         Text.Append (Fill (Field'Image'Length .. Fill'Last));
         Text.Append ("=> " & Results.all (Field)'Image & ASCII.LF);
      end loop;

      return To_String (Text);
   end Pretty_Print;

   -------------------
   -- Check_Results --
   -------------------

   procedure Check_Results
     (Errors          : Natural; Actual, Expected : Mold.Results_Access;
      Expected_Errors : Natural := 0)
   is
   begin
      Simple_Logging.Detail (Pretty_Print (Errors, Actual));

      Assert
        (Errors = Expected_Errors,
         "Incorrect Errors: reported" & Errors'Image & ", expected" &
         Expected_Errors'Image);

      for Field in Mold.Field_Type loop
         Assert
           (Actual.all (Field) = Expected.all (Field),
            "Wrong number of " & Field'Image & ": reported" &
            Actual.all (Field)'Image & ", expected" &
            Expected.all (Field)'Image);
      end loop;
   end Check_Results;

   ----------------
   -- MD5_Digest --
   ----------------

   function MD5_Digest (File_Name : String) return String is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      use GNAT.MD5;

      subtype Buffer_Type is Stream_Element_Array (1 .. 512);

      File : File_Type;
      C    : Context := Initial_Context;

      Buffer : Buffer_Type;
      Last   : Stream_Element_Offset := 0;
   begin
      File.Open (In_File, File_Name);

      loop
         exit when File.End_Of_File;
         File.Read (Buffer, Last);
         C.Update (Buffer (1 .. Last));
      end loop;

      File.Close;
      return Digest (C);
   end MD5_Digest;

   ----------------------
   -- Check_MD5_Digest --
   ----------------------

   procedure Check_MD5_Digest (File_Name, Digest : String) is
      use Ada.Directories;
   begin
      Assert (Exists (File_Name), "File not found: " & File_Name);
      Assert
        (MD5_Digest (File_Name) = Digest,
         "Invalid MD5 digest of file " & File_Name);
   end Check_MD5_Digest;

begin

   Simple_Logging.Level := Simple_Logging.Always;

end Support;
