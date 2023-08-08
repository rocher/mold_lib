-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

with Definitions;

package body Mold is

   package Dir renames Ada.Directories;

   use all type Dir.File_Kind;

   -----------
   -- Apply --
   -----------

   --!pp off
   function Apply
   (
      Source      : String          := ".";
      Output_Dir  : String          := "";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := null;
      Results     : Results_Access  := null
   )
   return Natural
   --!pp on

   is
   begin

      declare
         Output_Path : aliased constant String := Output_Dir;

         Used_Settings : constant Settings_Access :=
           (if Settings = null then Default_Settings'Access else Settings);
      begin

         if Results /= null then
            Results.all := [others => 0];
         end if;

         if Source'Length = 0 or else not Dir.Exists (Source)
           or else Dir.Kind (Source) = Dir.Special_File
         then
            return 1;
         end if;

         if Dir.Kind (Source) = Dir.Ordinary_File
           and then Dir.Extension (Source) /= File_Extension
         then
            return 1;
         end if;

         if not Dir.Exists (Output_Path) then
            Dir.Create_Path (Output_Path);
         elsif Dir.Kind (Output_Path) /= Dir.Directory then
            return 1;
         end if;

         if Definitions'Length = 0 or else not Dir.Exists (Definitions)
           or else Dir.Kind (Definitions) /= Dir.Ordinary_File
         then
            return 1;
         end if;

         declare
            Variables : aliased Standard.Definitions.Variables_Map;
            Success   : Boolean;
            Errors    : constant Natural := 0;
         begin

            Variables :=
              Standard.Definitions.Read_Variables
                (Definitions, Used_Settings, Results, Success);

            if not Success then
               return 1;
            end if;

            return Errors;
         end;
      end;

   exception
      when others =>
         return 1;

   end Apply;

end Mold;
