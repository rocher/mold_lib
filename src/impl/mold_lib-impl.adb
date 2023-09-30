-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Mold_Lib.Impl is

   ---------
   -- Inc --
   ---------

   procedure Inc (Results : Results_Access; Field : Results_Fields)
   is
   begin
      if Results /= null then
         Results.all (Field) := @ + 1;
      end if;
   end Inc;

begin
   --                                   .-------..----.
   --                                   |   3   ||  4 |
   Variable_Matcher.Compile ("(.*?)({{ *([^/} ]+)(/.*)? *}})");
   --                         | 1 ||           2           |
   --                         '---''-----------------------'
   --  Example:
   --
   --              1         2         3
   --     123456789012345678901234567890123456789
   --     This is a {{ #foo/0 }} variable example
   --
   --                     Matches (0) = ( 1, 22) = "This is a {{ #foo/0 }}"
   --     Pre_Text     := Matches (1) = ( 1, 10) = "This is a "
   --     Var_Mold     := Matches (2) = (11, 22) =           "{{ #foo/0 }}"
   --     Var_All_Name := Matches (3) = (14, 19) =              "#foo"
   --     Var_Name     := ( remove # if exists ) =               "foo"
   --     Filter       := Matches (4) = (18, 19) =                  "/0"

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
