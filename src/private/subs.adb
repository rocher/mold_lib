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

package body Subs is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;
   use all type Mold.Results_Access;

   ---------
   -- Inc --
   ---------

   procedure Inc (Results : Mold.Results_Access; Field : Mold.Field_Type) is
   begin
      if Results /= null then
         Results.all (Field) := @ + 1;
      end if;
   end Inc;

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
            Inc (Results, Mold.Variables);
         end loop;
      end if;

      return Vars;
   end Read_Variables_Map;

   -------------
   -- Replace --
   -------------

   function Replace
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
   end Replace;

end Subs;
