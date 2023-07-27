-------------------------------------------------------------------------------
--
--  MOLD - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with TOML;
with TOML.File_IO;
with Simple_Logging;

package body Subs is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;

   ------------------------
   -- Read_Variables_Map --
   ------------------------

   function Read_Variables_Map (Vars_File : String) return Variables_Map is
      use Variables_Package;

      Vars        : Variables_Map := Empty_Map;
      Read_Result : TOML.Read_Result;
   begin
      Read_Result := TOML.File_IO.Load_File (Vars_File);

      if Read_Result.Success then
         for Element of Read_Result.Value.Iterate_On_Table loop
            Vars.Include (Element.Key, Element.Value.As_Unbounded_String);
         end loop;
      end if;

      return Vars;
   end Read_Variables_Map;

   -------------
   -- Replace --
   -------------

   function Replace
     (Destination : String; Variables : Variables_Map;
      Action      : Mold.Undefined_Variable_Action;
      Alert       : Mold.Undefined_Variable_Alert) return Natural
   is
      CWD            : constant String := Dir.Current_Directory;
      Result         : Dir.Search_Type;
      Element        : Dir.Directory_Entry_Type;
      Replaced_Files : Natural         := 0;

   begin
      Log.Debug ("entering directory " & Destination);
      Dir.Set_Directory (Destination);

      Dir.Start_Search
        (Result, ".", "*",
         [Dir.Directory   => True, Dir.Ordinary_File => True,
         Dir.Special_File => False]);
      loop
         exit when not Result.More_Entries;

         Result.Get_Next_Entry (Element);
         declare
            Name      : constant String := Element.Simple_Name;
            Base_Name : constant String := Dir.Base_Name (Name);
            Extension : constant String := Dir.Extension (Name);
         begin
            if Name'Length > 0 and then Name /= "." and then Name /= ".."
              and then Name /= ".git"
            then
               Log.Debug ("entry name      = '" & Name & "'");
               Log.Debug ("entry basename  = '" & Base_Name & "'");
               Log.Debug ("entry extension = '" & Extension & "'");

               if Element.Kind = Dir.Directory then
                  Replaced_Files :=
                    @ + Replace (Name, Variables, Action, Alert);
               elsif Extension = "mold" then
                  Replaced_Files :=
                    @ + Replace_In_File (Name, Variables, Action, Alert);
               end if;
            end if;
         end;
      end loop;

      Dir.Set_Directory (CWD);
      return 0;
   end Replace;

end Subs;
