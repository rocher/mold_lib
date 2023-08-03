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
   Access_Variables : Replace.Variables_Access := Global_Variables'Access;
   Global_Errors    : Natural;

   ---------
   -- Run --
   ---------

   --!pp off
   function Apply
   (
      Source      : String          := ".";
      Output_Dir  : String          := ".";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := null;
      Results     : Results_Access  := null
   )
   return Natural
   --!pp on

   is

      Source_Alias     : aliased String := Source;
      Output_Dir_Alias : aliased String := Output_Dir;

      Used_Settings : constant Settings_Access :=
        (if Settings = null then Default_Settings'Access else Settings);

   begin
      if Results /= null then
         Results.all := [others => 0];
      end if;
      Global_Errors := 0;

      if Source'Length = 0 or else not Dir.Exists (Source)
        or else Dir.Kind (Source) = Dir.Special_File
      then
         Log.Error ("No such file or directory '" & Source & "'");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      if Dir.Kind (Source) = Dir.Ordinary_File
        and then Dir.Extension (Source) /= File_Extension
      then
         Log.Error ("Source file with Invalid extension");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      if Dir.Kind (Source) = Dir.Ordinary_File then
         if Output_Dir'Length = 0 or else not Dir.Exists (Output_Dir)
           or else Dir.Kind (Output_Dir) /= Dir.Directory
         then
            Log.Error ("Invalid output directory '" & Output_Dir & "'");
            Global_Errors := 1;
            goto Finalize_Function;
         end if;
      else
         if Output_Dir /= "." then
            Log.Error ("Invalid output directory '" & Output_Dir & "'");
            Global_Errors := 1;
            goto Finalize_Function;
         end if;
      end if;

      if Definitions'Length = 0 or else not Dir.Exists (Definitions)
        or else Dir.Kind (Definitions) /= Dir.Ordinary_File
      then
         Log.Error ("Definitions file not found");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      Global_Variables :=
        Replace.Read_Variables_Map (Definitions, Used_Settings, Results);
      if Global_Variables.Is_Empty then
         Log.Error ("Could not load a valid set of variables");
         Global_Errors := 1;
         goto Finalize_Function;
      end if;

      Global_Errors :=
        Replace.Apply
          (Source_Alias, Output_Dir_Alias, Access_Variables, Used_Settings,
           Results);
      Global_Variables.Clear;

      <<Finalize_Function>>

      return Global_Errors;
   end Apply;

end Mold;
