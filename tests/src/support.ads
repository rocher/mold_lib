-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development TESTS
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

with Mold_Lib;

package Support is

   package Mold renames Mold_Lib;

   --!pp off
   Default_Test_Settings : aliased constant Mold.Settings_Type :=
   (
      Replacement_In_File_Names    => True,
      Delete_Source_Files          => False,   --  Do not remove source files
      Overwrite_Destination_Files  => True,    --  Overwrite destination files
      Enable_Defined_Settings      => True,
      Undefined_Variable_Action    => Mold.Ignore,
      Undefined_Variable_Alert     => Mold.Warning,
      Undefined_Filter_Alert       => Mold.Warning,
      Abort_On_Error               => True
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
