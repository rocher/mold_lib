-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Simple_Logging;

with Replace;

package body Mold is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;

   Global_Variables : aliased Replace.Variables_Map;
   Global_Errors    : Natural;

   ---------
   -- Run --
   ---------

   function Apply
   --!pp off
   (
      Source      : String          := ".";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := Default_Settings'Access;
      Results     : Results_Access   := null
   )
   --!pp on
   return Natural
   is
   begin
      if Results /= null then
         Results.all := [others => 0];
      end if;
      Global_Errors := 0;

      if Source'Length = 0 or else not Dir.Exists (Source)
        or else Dir.Kind (Source) = Dir.Special_File
      then
         Log.Error ("No such file or directory");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      if Dir.Kind (Source) = Dir.Ordinary_File
        and then Dir.Extension (Source) /= Mold_File_Extension
      then
         Log.Error ("Source file with Invalid extension");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      if Definitions'Length = 0 or else not Dir.Exists (Definitions)
        or else Dir.Kind (Definitions) /= Dir.Ordinary_File
      then
         Log.Error ("Definitions file not found");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      Global_Variables := Replace.Read_Variables_Map (Definitions, Results);
      if Global_Variables.Is_Empty then
         Log.Error ("Could not load a valid set of variables");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      Global_Errors :=
        Replace.Apply (Source, Global_Variables'Access, Settings, Results);
      Global_Variables.Clear;

      <<Finalize_Function>>

      return Global_Errors;
   end Apply;

end Mold;
