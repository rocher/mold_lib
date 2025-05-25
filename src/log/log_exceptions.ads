-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Source_Info; use GNAT.Source_Info;

package Log_Exceptions is

   procedure Log_Exception
     (Occurrence : Exception_Occurrence;
      Message    : String := "";
      Location   : String := Source_Location;
      Entity     : String := Enclosing_Entity);

end Log_Exceptions;
