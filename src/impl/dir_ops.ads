-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with GNAT.Directory_Operations;

package Dir_Ops is

   function Path (A, B : String) return String is
     (A & GNAT.Directory_Operations.Dir_Separator & B);

   function Full_Path (A : String; B : String := "") return String is
     (Ada.Directories.Full_Name
        (GNAT.Directory_Operations.Format_Pathname
           (A & GNAT.Directory_Operations.Dir_Separator & B)));

   function Full_Path_Expanded (A : String; B : String := "") return String is
     (Ada.Directories.Full_Name
        (GNAT.Directory_Operations.Expand_Path
           (GNAT.Directory_Operations.Format_Pathname
              (A & GNAT.Directory_Operations.Dir_Separator & B))));

end Dir_Ops;
