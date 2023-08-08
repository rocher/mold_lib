-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold is

   type Substitution_Kind is (Normal, Optional, Mandatory);
   type Undef_Var_Action is (Ignore, Empty);
   type Undef_Var_Alert is (None, Warning);

   Optional_Substitution_Prefix  : constant Character := '?';
   Mandatory_Substitution_Prefix : constant Character := '#';
   File_Extension                : constant String    := "mold";
   Inclusion_File_Prefix         : constant String    := "include:";
   Include_File_Extension        : constant String    := "molt";
   Variable_Setting_Prefix       : constant String    := "mold-";

   type Settings_Type is record
      Rename_Source    : aliased Boolean;
      Delete_Source    : aliased Boolean;
      Overwrite        : aliased Boolean;
      Defined_Settings : aliased Boolean;
      Action           : aliased Undef_Var_Action;
      Alert            : aliased Undef_Var_Alert;
      Abort_On_Error   : aliased Boolean;
   end record;
   type Settings_Access is access all Settings_Type;

   --!pp off
   Default_Settings : aliased Settings_Type :=
   (
      Rename_Source    => True,
      Delete_Source    => True,
      Overwrite        => False,
      Defined_Settings => True,
      Action           => Ignore,
      Alert            => Warning,
      Abort_On_Error   => True
   );
   --!pp on

   --!pp off
   type Field_Type is
   (
      Files,
      Renamed,
      Overwritten,
      Definitions,
      Variables,
      Undefined,
      Replaced,
      Ignored,
      Emptied,
      Warnings,
      Errors
   );
   --!pp on

   type Results_Type is array (Field_Type) of Natural;
   type Results_Access is access all Results_Type;

   function Apply return Natural;

end Mold;
