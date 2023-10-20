-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold_Lib.Impl.File;

package body Mold_Lib.Impl.Directory is

   use all type Dir.File_Kind;

   -------------
   -- Replace --
   -------------

   --!pp off
   function Replace (
      Sub_Dir    :          String;
      Source     : not null String_Access;
      Output_Dir : not null String_Access
   )  return Natural
   --!pp on

   is
      Errors  : Natural         := 0;
      CWD     : constant String := Dir.Current_Directory;
      Result  : Dir.Search_Type;
      Element : Dir.Directory_Entry_Type;
   begin

      Log.Debug ("BEGIN Impl.Directory.Replace");
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
                       Output_Dir);
               elsif Dir.Extension (Name) = Mold_File_Extension then
                  declare
                     Out_Sub_dir : aliased String :=
                       Path (Output_Dir.all, Sub_Dir);
                  begin
                     Log.Debug ("File replace in element " & Name);
                     Errors :=
                       Errors +
                       Impl.File.Replace
                         (Name'Unchecked_Access, Out_Sub_dir'Unchecked_Access);
                  end;
               end if;
               if Errors > 0 and then Args.Settings.Abort_On_Error then
                  goto Exit_Function;
               end if;
            end if;
         end;
      end loop;

      <<Exit_Function>>

      Dir.Set_Directory (CWD);
      Log.Debug ("END Impl.Directory.Replace");
      return Errors;

   exception
      when Dir.Name_Error =>
         Log.Error ("EXCEPTION caught Name_Error");
         Dir.Set_Directory (CWD);
         Errors := @ + 1;
         return Errors;

   end Replace;

end Mold_Lib.Impl.Directory;
