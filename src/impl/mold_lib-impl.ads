-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with GNAT.Directory_Operations;
with GNAT.Regpat;

package Mold_Lib.Impl is

   package Dir renames Ada.Directories;
   package Reg renames GNAT.Regpat;

   Optional_Replacement_Prefix  : constant Character := '?';
   Mandatory_Replacement_Prefix : constant Character := '#';
   Mold_File_Extension          : constant String    := "mold";
   Inclusion_Prefix             : constant String    := "include:";
   Include_File_Extension       : constant String    := "molt";
   Defined_Setting_Prefix       : constant String    := "mold-";

   Variable_Matcher : Reg.Pattern_Matcher (256);
   File_Matcher     : Reg.Pattern_Matcher (256);
   Include_Matcher  : Reg.Pattern_Matcher (128);
   --  Regular expressions to match variables in files and filenames, and
   --  including templates. These variables are initialized in the package
   --  body.

   procedure Inc (Results : Results_Access; Field : Results_Fields);
   --  If Results is not null, increment the given Field in Results.

   function Path (A, B : String) return String is
     (A & GNAT.Directory_Operations.Dir_Separator & B);
   --  Return the path obtained by the concatenation of the directory A and
   --  the file or directory B: in Unix systems, "A/B"

   function Full_Path (A : String; B : String := "") return String is
     (Dir.Full_Name
        (GNAT.Directory_Operations.Format_Pathname
           (A & GNAT.Directory_Operations.Dir_Separator & B)));
   --  Return the full path obtained by the concatenation of the directory A
   --  and the file or directory B: in Unix systems, "/<PATH_TO_A>/A/B". If B
   --  is the empty string, return the pathname expansion of A.

   function Full_Path_Expanded (A : String; B : String := "") return String is
     (Dir.Full_Name
        (GNAT.Directory_Operations.Expand_Path
           (GNAT.Directory_Operations.Format_Pathname
              (A & GNAT.Directory_Operations.Dir_Separator & B))));
   --  Return the full path obtained by the concatenation of the directory A
   --  and then file or directory B, expanding environment variables: in Unix
   --  systems, "$HOME/A/B" --> "/home/user/<PATH_TO_A>/B". If B is the empty
   --  string, return the pathname expansion of A.

end Mold_Lib.Impl;
