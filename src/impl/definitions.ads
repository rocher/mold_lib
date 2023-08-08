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

with Mold;

package Definitions is

   package Variables_Package is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Unbounded_String,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=", "=" => "=");

   subtype Variables_Map is Variables_Package.Map;
   type Variables_Access is access all Variables_Map;

   --!pp off
   function Read_Variables
   (
      Vars_File :          String;
      Settings  : not null Mold.Settings_Access;
      Results   :          Mold.Results_Access := null;
      Success   : out      Boolean
   )
   return Variables_Map;
   --!pp on
   --
   --  Read all Definitions definition of the given TOML Vars_File. Return a
   --  Variables_Map object.
   --  ------------------------------------------------------------------------

end Definitions;
