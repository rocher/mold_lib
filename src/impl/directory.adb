-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Simple_Logging;

with Dir_Ops; use Dir_Ops;
with File;

package body Directory is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Kind;

   -------------
   -- Replace --
   -------------

   --!pp off
   function Replace
   (
      Sub_Dir    :          String;
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Definitions.Variables_Access;
      Settings   : not null Mold.Settings_Access;
      Results    :          Mold.Results_Access := null
   )
   return Natural
   --!pp on

   is
      Errors  : Natural         := 0;
      CWD     : constant String := Dir.Current_Directory;
      Result  : Dir.Search_Type;
      Element : Dir.Directory_Entry_Type;
   begin

      Log.Debug ("REPLACE in directory");
      Log.Debug ("  Sub_Dir     : " & Sub_Dir);
      Log.Debug ("  Source      : " & Source.all);
      Log.Debug ("  Output_Dir  : " & Output_Dir.all);
      Log.Debug ("  CWD         : " & Dir.Current_Directory);
      Log.Debug ("  entering    : " & Source.all);
      Dir.Set_Directory (Source.all);
      Log.Debug ("  CWD         : " & Dir.Current_Directory);

      Dir.Start_Search
        (Result, ".", "*",
         [Dir.Directory   => True, Dir.Ordinary_File => True,
         Dir.Special_File => False]);

      loop
         exit when not Result.More_Entries;
         Result.Get_Next_Entry (Element);
         declare
            Name : aliased String := Element.Simple_Name;
         begin

            Log.Debug ("Dir Element : " & Name);

            if Name'Length > 0 and then Name /= "." and then Name /= ".."
              and then Name /= ".git"
            then
               if Element.Kind = Dir.Directory then
                  Log.Debug ("Directory replace in element " & Name);
                  Errors :=
                    Errors +
                    Replace
                      (Dir.Compose (Sub_Dir, Name), Name'Unchecked_Access,
                       Output_Dir, Variables, Settings, Results);
               elsif Dir.Extension (Name) = Mold.Mold_File_Extension then
                  declare
                     Out_Sub_dir : aliased String :=
                       Path (Output_Dir.all, Sub_Dir);
                  begin
                     Log.Debug ("File replace in element " & Name);
                     Errors :=
                       Errors +
                       File.Replace
                         (Name'Unchecked_Access, Out_Sub_dir'Unchecked_Access,
                          Variables, Settings, Results);
                  end;
               end if;
               if Errors > 0 and then Settings.Abort_On_Error then
                  goto Exit_Function;
               end if;
            end if;
         end;
      end loop;

      <<Exit_Function>>

      Dir.Set_Directory (CWD);
      return Errors;

   exception
      when Dir.Name_Error =>
         Log.Error ("EXCEPTION caught Name_Error");
         Dir.Set_Directory (CWD);
         Errors := @ + 1;
         return Errors;

   end Replace;

end Directory;
