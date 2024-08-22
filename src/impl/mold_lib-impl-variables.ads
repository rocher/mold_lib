-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Impl.Variables is

   function Read
     (Toml_Path : String; Success : out Boolean) return Variables_Map;
   --  Read all variable definitions of the given TOML Toml_Path. Return a
   --  Variables_Map object.

   function Apply_Variable_Substitution
     (Variables : in out Variables_Map) return Boolean;
   --  Applies the variable substitution process to all variables and update
   --  their value, if it has changed. Return True no error is found during the
   --  the variable substitution process.

   function Get_Value (Variable : String) return String;
   --  Return the value of Variable, if defined. When undefined, it reports an
   --  error and behaves according to the settings.

end Mold_Lib.Impl.Variables;
