-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------
--!pp off

package Mold_Lib.Impl.Validation is

   function Valid_Input_Paths (
      Source      :     String;
      Output_Dir  :     String;
      Toml_File   :     String;
      Source_Path : out Unbounded_String;
      Output_Path : out Unbounded_String;
      Toml_Path   : out Unbounded_String
   )  return Boolean;
   --  Validate that Source files or directory exists, Output_Dir exists and
   --  Toml_File is a valid file, and set all Path variables. When no error
   --  found, return True. On error, report the problem(s) encountered and
   --  return False.

end Mold_Lib.Impl.Validation;
