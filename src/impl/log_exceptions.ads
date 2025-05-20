-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.Source_Info;

package Log_Exceptions is

   --!pp off
   procedure Log_Exception
   (
      Occurrence : Ada.Exceptions.Exception_Occurrence;
      Message    : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity
   );
   --!pp on

end Log_Exceptions;
