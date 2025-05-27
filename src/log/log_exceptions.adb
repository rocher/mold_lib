-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging; use Simple_Logging;

package body Log_Exceptions is

   package Log renames Simple_Logging;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Occurrence : Exception_Occurrence;
      Message    : String := "";
      Location   : String := Source_Location;
      Entity     : String := Enclosing_Entity)
   is
      Exception_Text : constant String :=
        (if Log.Level = Log.Debug
         then ("(EXCEPTION " & Exception_Name (Occurrence) & "): ")
         else "");
      Location_Text  : constant String :=
        (if Log.Level = Log.Debug
         then ", in " & Location & " (" & Entity & ")"
         else "");
   begin
      Log.Error
        (Exception_Text
         & Message
         & (if Message /= "" then "," else "")
         & Exception_Message (Occurrence)
         --  & " '"
         & Location_Text);
   end Log_Exception;

end Log_Exceptions;
