-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

with Lib_Mold;

package Support is

   package Mold renames Lib_Mold;

   --!pp off
   Default_Test_Settings : aliased constant Mold.Settings_Type :=
   (
      Rename_Source    => True,
      Delete_Source    => False,   --  Do not remove source files
      Overwrite        => True,    --  Overwrite destination files
      Defined_Settings => True,
      Action           => Mold.Ignore,
      Alert            => Mold.Warning,
      Abort_On_Error   => True
   );
   --!pp on

   Global_Settings : constant Mold.Settings_Access :=
     Default_Test_Settings'Unrestricted_Access;

   --  These are global Support used for testing. Important thing is to not
   --  to remove source files, except in tests prepared for this.

   function Pretty_Print
     (Errors : Natural; Results : Mold.Results_Access) return String;

   --!pp off
   procedure Check_Results
   (
      Errors             : Natural;
      Reported, Expected : Mold.Results_Access;
      Expected_Errors    : Natural := 0;
      Source             : String  := GNAT.Source_Info.File;
      Line               : Natural := GNAT.Source_Info.Line
   );
   --!pp on

   procedure Check_MD5_Digest (File_Name, Digest : String);

end Support;
