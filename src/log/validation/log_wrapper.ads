-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;
with Simple_Logging;

package Log_Wrapper is

   procedure Log_Debug
     (Msg      : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location)
   renames Simple_Logging.Debug;
   --  Keep Log_Debug code in the validation build.

end Log_Wrapper;
