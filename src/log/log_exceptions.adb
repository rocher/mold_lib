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

   procedure Log_Exception
     (Occurrence : Exception_Occurrence;
      Message    : String := "";
      Location   : String := Source_Location;
      Entity     : String := Enclosing_Entity) is
   begin
      Log.Error
        ("(EXCEPTION "
         & Exception_Name (Occurrence)
         & "): "
         & Message
         & (if Message /= "" then "," else "")
         & " '"
         & Exception_Message (Occurrence)
         & "', in "
         & Location
         & " ("
         & Entity
         & ")");
   end Log_Exception;

end Log_Exceptions;
