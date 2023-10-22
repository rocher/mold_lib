-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Directories;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with GNAT.Directory_Operations;
with GNAT.Regpat;

with Simple_Logging;

package Mold_Lib.Impl is

   package Dir renames Ada.Directories;
   package IO renames Ada.Text_IO;
   package Log renames Simple_Logging;
   package Reg renames GNAT.Regpat;

   --  MOLD CONSTANTS ---------------------------------------------------------

   Optional_Replacement_Prefix  : constant Character := '?';
   Mandatory_Replacement_Prefix : constant Character := '#';
   Mold_File_Extension          : constant String    := "mold";
   Inclusion_Prefix             : constant String    := "include:";
   Include_File_Extension       : constant String    := "molt";
   Defined_Setting_Prefix       : constant String    := "mold-";

   --  VARIABLES FOR DEFINED SETTINGS -----------------------------------------

   package Variables_Package is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Unbounded_String,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=", "=" => "=");

   subtype Variables_Map is Variables_Package.Map;
   type Variables_Access is access all Variables_Map;

   --  TRACKING INCLUDED FILES ------------------------------------------------

   package Inclusion_Package is new Doubly_Linked_Lists
     (Unbounded_String, "=");

   subtype Inclusion_List is Inclusion_Package.List;
   --  List of included files to avoid circular references.

   --  ARGUMENTS --------------------------------------------------------------

   type Arguments_Type is record
      Running_Directory : Unbounded_String := Null_Unbounded_String;
      Source            : String_Access;
      Variables         : Variables_Access;
      Settings          : Settings_Access;
      Filters           : Filters_Access;
      Results           : Results_Access;
      Errors            : Natural;
      Included_Files    : Inclusion_List;
   end record;
   --  Arguments used during the variable substitution process:
   --
   --     Running_Directory   Directory from where mold has been launched
   --     Source              Source file currently being processed
   --     Variables           Set of variables defined, either in the toml
   --                         file or in a source file
   --     Settings            Set of settings used in the process
   --     Filters             Set of custom filters
   --     Results             Set of results from the process
   --     Included_Files      List of included files, to avoid circular refs.

   Args : Arguments_Type;

   --  REGULAR EXPRESSIONS ----------------------------------------------------

   Variable_Matcher : Reg.Pattern_Matcher (256);
   File_Matcher     : Reg.Pattern_Matcher (256);
   Include_Matcher  : Reg.Pattern_Matcher (128);
   --  Regular expressions to match variables in files and filenames, and
   --  including templates. These variables are initialized in the package
   --  body.

   --  UTILITY FUNCTIONS ------------------------------------------------------

   procedure Inc_Result (Field : Results_Fields; Amount : Natural := 1);
   --  Increment the Field in Args.Results with the Amount.

   function Path (A, B : String) return String is
     (A & GNAT.Directory_Operations.Dir_Separator & B);
   --  Return the path obtained by the concatenation of the directory A and
   --  the file or directory B: in Unix systems, "A/B"

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
