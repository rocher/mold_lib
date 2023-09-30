-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Mold_Lib.Impl.Definitions is

   package Variables_Package is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Unbounded_String,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=", "=" => "=");

   subtype Variables_Map is Variables_Package.Map;
   type Variables_Access is access all Variables_Map;

   --!pp off
   function Read_Variables
   (
      Vars_File :          String;
      Settings  : not null Settings_Access;
      Results   :          Results_Access := null;
      Success   : out      Boolean
   )
   return Variables_Map;
   --!pp on
   --
   --  Read all variable definitions of the given TOML Vars_File. Return a
   --  Variables_Map object.
   --  ------------------------------------------------------------------------

end Mold_Lib.Impl.Definitions;
