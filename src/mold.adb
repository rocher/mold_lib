-------------------------------------------------------------------------------
--
--  MOLD - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Git;
with Subs;

package body Mold is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   ---------
   -- Run --
   ---------

   function Run
   --!pp off
   (
      Repository  : String;
      Destination : String;
      Vars_File   : String                    := "mold.toml";
      Branch      : String                    := "main";
      Action      : Undefined_Variable_Action := Ignore;
      Alert       : Undefined_Variable_Alert  := Warning;
      Log_Level   : Simple_Logging.Levels     := Simple_Logging.Info
   )
   --!pp on
   return Natural
   is
   begin
      Log.Level := Log_Level;

      if not Dir.Exists (Vars_File) then
         Log.Error ("Variables file not found");
         return 0;
      end if;

      if Repository'Length = 0 then
         if not Dir.Exists (Destination) then
            Log.Error ("Destination directory not found");
            return 0;
         end if;
      else
         if not Git.Clone (Repository, Destination, Branch) then
            Log.Error ("Could not clone repository/branch");
            return 0;
         end if;
      end if;

      declare
         Variables : constant Subs.Variables_Map :=
           Subs.Read_Variables_Map (Vars_File);
      begin
         if Variables.Is_Empty then
            Log.Error ("Could not load a valid set of variables");
            return 0;
         else
            return Subs.Replace (Destination, Variables, Action, Alert);
         end if;
      end;
   end Run;

end Mold;
