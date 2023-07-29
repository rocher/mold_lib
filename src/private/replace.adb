-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with TOML;
with TOML.File_IO;
with Simple_Logging;

with Directory;
with File;
with Results; use Results;

package body Replace is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;
   use all type Mold.Results_Access;

   ------------------------
   -- Read_Variables_Map --
   ------------------------

   function Read_Variables_Map
     (Vars_File : String; Results : Mold.Results_Access) return Variables_Map
   is
      use Variables_Package;

      Vars        : Variables_Map := Empty_Map;
      Read_Result : TOML.Read_Result;
   begin
      Read_Result := TOML.File_IO.Load_File (Vars_File);

      if Read_Result.Success then
         for Element of Read_Result.Value.Iterate_On_Table loop
            Vars.Include (Element.Key, Element.Value.As_Unbounded_String);
            Log.Debug
              ("defined var " & To_String (Element.Key) & " = " &
               Element.Value.As_String);
            Inc (Results, Mold.Defined);
         end loop;
      else
         Log.Debug ("Error reading definitions file");
      end if;

      return Vars;
   end Read_Variables_Map;

   -----------
   -- Apply --
   -----------

   function Apply
   --!pp off
   (
      Source    : String;
      Variables : Variables_Access;
      Settings  : Mold.Settings_Access;
      Results   : Mold.Results_Access
   )
   --!pp on

      return Natural
   is
      Errors : Natural := 0;
   begin
      if Dir.Kind (Source) = Dir.Ordinary_File then
         Errors := File.Replace (Source, Variables, Settings, Results);
      else
         Errors := Directory.Replace (Source, Variables, Settings, Results);
      end if;

      return Errors;
   end Apply;

end Replace;
