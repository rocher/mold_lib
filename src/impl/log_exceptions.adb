-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package body Log_Exceptions is

   package Log renames Simple_Logging;

   -------------------
   -- Log_Exception --
   -------------------

   --!pp off
   procedure Log_Exception
   (
      Occurrence : Ada.Exceptions.Exception_Occurrence;
      Message    : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity
   )
   --!pp on

   is
      use Ada.Exceptions;
   begin
      Log.Error
        ("(EXCEPTION " & Exception_Name (Occurrence) & "): " & Message &
         (if Message /= "" then "," else "") & " '" &
         Exception_Message (Occurrence) & "', in " & Location & " (" & Entity &
         ")");
   end Log_Exception;

end Log_Exceptions;
