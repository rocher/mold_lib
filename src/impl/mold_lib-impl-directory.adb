-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Log_Exceptions; use Log_Exceptions;
with Log_Wrapper; use Log_Wrapper;
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
   )  return Boolean
   --!pp on

   is
      Success : Boolean := True;
      --  return True if no replacement has been made; non-regular files found
      --  or no mold files in the current directory

      CWD     : constant String := Dir.Current_Directory;
      Result  : Dir.Search_Type;
      Element : Dir.Directory_Entry_Type;
   begin

      Log.Detail ("processing directory " & Source.all);

      Log_Debug ("BEGIN Impl.Directory.Replace");
      Log_Debug ("  Sub_Dir     : " & Sub_Dir);
      Log_Debug ("  Source      : " & Source.all);
      Log_Debug ("  Output_Dir  : " & Output_Dir.all);
      Log_Debug ("  CWD         : " & Dir.Current_Directory);
      Log_Debug ("  entering    : " & Source.all);
      Dir.Set_Directory (Source.all);
      Log_Debug ("  CWD         : " & Dir.Current_Directory);

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

            Log_Debug ("Dir Element : " & Name);

            if Name'Length > 0 and then Name /= "." and then Name /= ".."
              and then Name /= ".git"
            then
               if Element.Kind = Dir.Directory then
                  Log_Debug ("Directory replace in element " & Name);
                  Success :=
                    Replace
                      (Dir.Compose (Sub_Dir, Name), Name'Unchecked_Access,
                       Output_Dir);
               elsif Dir.Extension (Name) = Mold_File_Extension then
                  declare
                     Out_Sub_dir : aliased String :=
                       Path (Output_Dir.all, Sub_Dir);
                  begin
                     Log_Debug ("File replace in element " & Name);
                     Success :=
                       Impl.File.Replace
                         (Name'Unchecked_Access, Out_Sub_dir'Unchecked_Access);
                  end;
               end if;
               if not Success then
                  goto Exit_Function;
               end if;
            end if;
         end;
      end loop;

      <<Exit_Function>>

      Dir.Set_Directory (CWD);
      Log_Debug ("END Impl.Directory.Replace");
      return Success;

      pragma Annotate (Xcov, Exempt_On, "Only valid in Windows OS");
   exception
      when E : Dir.Name_Error | Dir.Use_Error =>
         Log_Exception (E);
         Dir.Set_Directory (CWD);
         return False;
         pragma Annotate (Xcov, Exempt_Off);

   end Replace;

end Mold_Lib.Impl.Directory;
