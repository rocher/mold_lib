-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------
--!pp off

package Mold_Lib.Impl.Directory is

   function Replace (
      Sub_Dir    :          String;
      Source     : not null String_Access;
      Output_Dir : not null String_Access
   )  return Boolean;
   --  Recursively apply variable replacement to all mold files (with
   --  extension "mold") in all sub-directories, starting at directory Source.
   --
   --  Return True if the process end successfully (no error detected).

end Mold_Lib.Impl.Directory;
