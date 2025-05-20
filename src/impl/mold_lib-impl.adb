-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Mold_Lib.Impl is

   ---------
   -- Inc --
   ---------

   procedure Inc_Result (Field : Results_Fields; Amount : Natural := 1) is
   begin
      if Args.Results /= null then
         Args.Results.all (Field) := @ + Amount;
      end if;
   end Inc_Result;

begin
   --                                   .-------..------.
   --                                   |   3   ||   4  |
   Variable_Matcher.Compile ("(.*?)({{ *([^/} ]+)(/[^}]*)? *}})");
   --                         | 1 ||             2            |
   --                         '---''--------------------------'
   --  Example:
   --
   --              1         2         3
   --     123456789012345678901234567890123456789
   --     This is a {{ #foo/s }} variable example
   --
   --                     Matches (0) = ( 1, 22) = "This is a {{ #foo/s }}"
   --     Pre_Text     := Matches (1) = ( 1, 10) = "This is a "
   --     Var_Mold     := Matches (2) = (11, 22) =           "{{ #foo/s }}"
   --     Var_All_Name := Matches (3) = (14, 19) =              "#foo"
   --     Var_Name     := ( remove # if exists ) =               "foo"
   --     Filter       := Matches (4) = (18, 20) =                  "/s "

   --                             .------.
   --                             |   3  |
   File_Matcher.Compile ("(.*?)(__([^_]+?)__)");
   --                     | 1 ||     2      |
   --                     '---''------------'
   --  Example:
   --
   --              1         2
   --     123456789012345678901
   --     README-__PURPOSE__.md
   --
   --                     Matches (0) = ( 1, 18) = "README-__PURPOSE__"
   --     Pre_Text     := Matches (1) = ( 1,  7) = "README-"
   --     Var_Mold     := Matches (2) = ( 8, 18) =        "__PURPOSE__"
   --     Var_All_Name := Matches (3) = (10, 16) =          "PURPOSE"

   Include_Matcher.Compile ("^{{ *" & Inclusion_Prefix & "([^ ]+) *}}$");
   --                                                     |  1  |
   --                                                     '-----'
   --  Example:
   --
   --              1         2
   --     1234567890123456789012
   --     {{ include:foo.molt }}
   --
   --                     Matches (0) = ( 1, 22) = "{{ include:foo.molt }}"
   --     Var_Name     := Matches (1) = (12, 19) =            "foo.molt"

end Mold_Lib.Impl;
