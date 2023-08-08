-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

with Definitions;
--  with Directory;
with Dir_Ops; use Dir_Ops;
--  with File;

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
      Source_Alias : aliased String := Source;
   begin

      declare
         Output_Path : aliased String :=
           Full_Path_Expanded
             (if Output_Dir'Length > 0 then Output_Dir else Source);

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
            Errors    : Natural;
         begin

            Variables :=
              Standard.Definitions.Read_Variables
                (Definitions, Used_Settings, Results, Success);

            if not Success then
               return 1;
            end if;

            --  if Dir.Kind (Source) = Dir.Ordinary_File then
            --     Errors :=
            --       File.Replace
            --         (Source_Alias'Unrestricted_Access,
            --          Output_Path'Unrestricted_Access,
            --          Variables'Unchecked_Access, Settings, Results);
            --  else
            --     File.Set_Running_Directory (Dir.Current_Directory);
            --     Errors :=
            --       Directory.Replace
            --         ("", Source_Alias'Unrestricted_Access,
            --          Output_Path'Unrestricted_Access,
            --          Variables'Unchecked_Access, Settings, Results);
            --  end if;

            return Errors;
         end;
      end;

   exception
      when others =>
         return 1;

   end Apply;

end Mold;
