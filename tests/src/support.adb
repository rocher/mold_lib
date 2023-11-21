-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.MD5;

with AUnit.Assertions; use AUnit.Assertions;
with Simple_Logging;
with Simple_Logging.Decorators;

with Mold_Lib_Tests_Config; use Mold_Lib_Tests_Config;

package body Support is

   ------------------
   -- Pretty_Print --
   ------------------

   function Pretty_Print
     (Success : Boolean; Results : Mold.Results_Access) return String
   is
      Fill : constant String  := "           ";
      Text : Unbounded_String := To_Unbounded_String ("");
   begin
      Text.Append (ASCII.LF & "    Success:" & Success'Image);
      Text.Append (ASCII.LF & "    Results:" & ASCII.LF);

      for Field in Mold.Results_Fields loop
         Text.Append ("       " & Field'Image);
         Text.Append (Fill (Field'Image'Length .. Fill'Last));
         Text.Append ("=> " & Results.all (Field)'Image & ASCII.LF);
      end loop;

      return To_String (Text);
   end Pretty_Print;

   --!pp off
   procedure Check_Success
     (Reported, Expected : Boolean;
      Source             : String := GNAT.Source_Info.File;
      Line               : Natural := GNAT.Source_Info.Line)
   --!pp on

   is
   begin
      Assert
        (Reported = Expected,
         "Incorrect Success: reported " & Reported'Image & ", expected " &
         Expected'Image,
         Source, Line);
   end Check_Success;

   -------------------
   -- Check_Results --
   -------------------

   --!pp off
   procedure Check_Results (
      Success            : Boolean;
      Expected_Success   : Boolean;
      Reported, Expected : Mold.Results_Access;
      Source             : String  := GNAT.Source_Info.File;
      Line               : Natural := GNAT.Source_Info.Line)
   --!pp on

   is
   begin
      Simple_Logging.Detail (Pretty_Print (Success, Expected));

      Assert
        (Success = Expected_Success,
         "Incorrect Success: reported " & Success'Image & ", expected " &
         Expected_Success'Image,
         Source, Line);

      for Field in Mold.Results_Fields loop
         Assert
           (Reported.all (Field) = Expected.all (Field),
            "Wrong number of " & Field'Image & ": reported" &
            Reported.all (Field)'Image & ", expected" &
            Expected.all (Field)'Image,
            Source, Line);
      end loop;
   end Check_Results;

   ----------------
   -- MD5_Digest --
   ----------------

   function MD5_Digest (Filename : String) return String is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      use GNAT.MD5;

      subtype Buffer_Type is Stream_Element_Array (1 .. 512);

      File : File_Type;
      C    : Context := Initial_Context;

      Buffer : Buffer_Type;
      Last   : Stream_Element_Offset := 0;
   begin
      File.Open (In_File, Filename);

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

   procedure Check_MD5_Digest (Filename, Unix_Digest, DOS_Digest : String) is
      use Ada.Directories;
   begin
      Assert (Exists (Filename), "File not found: " & Filename);

      if Alire_Host_OS in "windows" then
         Assert
           (MD5_Digest (Filename) = DOS_Digest,
            "Invalid MD5 digest of file " & Filename);
      else
         Assert
           (MD5_Digest (Filename) = Unix_Digest,
            "Invalid MD5 digest of file " & Filename);
      end if;
   end Check_MD5_Digest;

begin

   Simple_Logging.Level                         := Simple_Logging.Always;
   Simple_Logging.Decorators.Location_Decorator :=
     Simple_Logging.Decorators.No_Location_Decorator'Access;

   --  Simple_Logging.Level                         := Simple_Logging.Debug;
   --  Simple_Logging.Decorators.Location_Decorator :=
   --    Simple_Logging.Decorators.Simple_Location_Decorator'Access;

end Support;
