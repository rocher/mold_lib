-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Definitions is

   package Variables_Package is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Unbounded_String,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=", "=" => "=");

   subtype Variables_Map is Variables_Package.Map;
   type Variables_Access is access all Variables_Map;

   function Read_Variables return Variables_Map;

end Definitions;
