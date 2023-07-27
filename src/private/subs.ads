-------------------------------------------------------------------------------
--
--  MOLD - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Mold;

package Subs is

   package Variables_Package is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Unbounded_String,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=", "=" => "=");

   subtype Variables_Map is Variables_Package.Map;

   function Read_Variables_Map (Vars_File : String) return Variables_Map;

   function Replace
     (Destination : String; Variables : Variables_Map;
      Action      : Mold.Undefined_Variable_Action;
      Alert       : Mold.Undefined_Variable_Alert) return Natural;

end Subs;
