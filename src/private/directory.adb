-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Simple_Logging;

with File;

package body Directory is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;

   -------------
   -- Replace --
   -------------

   function Replace
   --!pp off
   (
      Name      : String;
      Variables : Standard.Replace.Variables_Access;
      Settings  : Mold.Settings_Access;
      Results   : Mold.Results_Access
   )
   --!pp on

      return Natural
   is
      Errors  : Natural         := 0;
      CWD     : constant String := Dir.Current_Directory;
      Result  : Dir.Search_Type;
      Element : Dir.Directory_Entry_Type;
   begin
      Log.Debug ("entering directory " & Name);
      Dir.Set_Directory (Name);

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
                  Errors := Replace (Name, Variables, Settings, Results);
               elsif Extension = "mold" then
                  Errors := File.Replace (Name, Variables, Settings, Results);
               end if;
            end if;
         end;
      end loop;

      Dir.Set_Directory (CWD);
      return Errors;
   end Replace;

end Directory;
