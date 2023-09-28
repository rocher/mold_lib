-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;

with Simple_Logging;

package body Text_Filters is

   package CHAR renames Ada.Characters.Handling;
   package Log renames Simple_Logging;

   Custom_Filters : Filters_Access := null;

   -----------------------------
   -- Set_Custom_Text_Filters --
   -----------------------------

   procedure Set_Custom_Text_Filters (Text_Filters : not null Filters_Access)
   is
   begin
      Custom_Filters := Text_Filters;
   end Set_Custom_Text_Filters;

   -----------
   -- Apply --
   -----------

   function Apply
     (Filters :     String; Value : String; Output : IO.File_Type;
      Summary : out Results_Type; Errors : Boolean := True) return UString
   is
      pragma Unreferenced (Output, Errors);
      Filter_Parsed : Text_Filter_Parsed;

      Result : UString := To_Unbounded_String (Value);
      Filter : UString := To_Unbounded_String (Filters);
      Tail   : UString;
   begin
      Summary := (others => 0);
      loop
         Filter_Parsed := Parse (Filter, Tail);
         Result        := Apply (Filter_Parsed, Result);
         Filter        := Tail;

         Log.Debug ("Filter_Parsed : " & Filter_Parsed'Image);
         Log.Debug ("Tail          : '" & Tail'Image & "'");
         Log.Debug ("Result        : '" & To_String (Result) & "'");

         --  exit when Filter_Parsed.Kind in filter_none | filter_error;
         exit when Tail = Null_Unbounded_String;
      end loop;

      return Result;
   end Apply;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filters : UString; Tail : out UString)
      return Text_Filter_Parsed is separate;

   -----------
   -- Apply --
   -----------

   function Apply
     (Filter : Text_Filter_Parsed; S : UString) return UString is separate;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (C : Character) return Boolean is
     (CHAR.Is_Space (C) or else C = ASCII.HT);

   ---------------
   -- Trim_Left --
   ---------------

   function Trim_Left (S : UString) return UString is
      First_Char : Natural := 1;
   begin
      loop
         exit when not Is_Blank (S.Element (First_Char));
         First_Char := First_Char + 1;
      end loop;
      return S.Unbounded_Slice (First_Char, S.Length);
   end Trim_Left;

   ----------------
   -- Trim_Right --
   ----------------

   function Trim_Right (S : UString) return UString is
      Last_Char : Natural := S.Length;
   begin
      loop
         exit when not Is_Blank (S.Element (Last_Char));
         Last_Char := Last_Char - 1;
      end loop;
      return S.Unbounded_Slice (1, Last_Char);
   end Trim_Right;

   ---------------
   -- Trim_Both --
   ---------------

   function Trim_Both (S : UString) return UString is
     (Trim_Left (Trim_Right (S)));

   -----------------
   -- Trim_Squash --
   -----------------

   function Trim_Squash (S : UString) return UString is
      Trimmed   : constant UString := S;
      Add_Space : Boolean          := True;
   begin
      return Aux : UString := To_Unbounded_String ("") do
         for I in 1 .. Trimmed.Length loop
            if Is_Blank (Trimmed.Element (I)) then
               if Add_Space then
                  Aux := Aux & ' ';
               end if;
               Add_Space := False;
            else
               Aux       := Aux & Trimmed.Element (I);
               Add_Space := True;
            end if;
         end loop;
      end return;
   end Trim_Squash;

   --------------
   -- Trim_All --
   --------------

   function Trim_All (S : UString) return UString is
     (Trim_Both (Trim_Squash (S)));

   -------------------
   -- Remove_Blanks --
   -------------------

   function Remove_Blanks (S : UString) return UString is
   begin
      return Aux : UString := To_Unbounded_String ("") do
         for I in 1 .. S.Length loop
            if not Is_Blank (S.Element (I)) then
               Aux := Aux & S.Element (I);
            end if;
         end loop;
      end return;
   end Remove_Blanks;

   -----------------
   -- Replace_All --
   -----------------

   function Replace_All (S : UString; Src, Dst : Character) return UString is
   begin
      return Aux : UString := S do
         for I in 1 .. Aux.Length loop
            if Aux.Element (I) = Src then
               Aux.Replace_Element (I, Dst);
            end if;
         end loop;
      end return;
   end Replace_All;

   -------------------
   -- Replace_First --
   -------------------

   function Replace_First (S : UString; Src, Dst : Character) return UString is
   begin
      return Aux : UString := S do
         for I in 1 .. Aux.Length loop
            if Aux.Element (I) = Src then
               Aux.Replace_Element (I, Dst);
               exit;
            end if;
         end loop;
      end return;
   end Replace_First;

   ------------------
   -- Replace_Last --
   ------------------

   function Replace_Last (S : UString; Src, Dst : Character) return UString is
   begin
      return Aux : UString := S do
         for I in reverse 1 .. Aux.Length loop
            if Aux.Element (I) = Src then
               Aux.Replace_Element (I, Dst);
               exit;
            end if;
         end loop;
      end return;
   end Replace_Last;

   --------------
   -- Sequence --
   --------------

   function Sequence (S : UString; Dst : Character) return UString is
     (S.Length * Dst);

   ----------------
   -- Delete_All --
   ----------------

   function Delete_All (S : UString; Src : Character) return UString is
   begin
      return Aux : UString := To_Unbounded_String ("") do
         for I in 1 .. S.Length loop
            if S.Element (I) /= Src then
               Aux := Aux & S.Element (I);
            end if;
         end loop;
      end return;
   end Delete_All;

   --------------------
   -- Case_Lowercase --
   --------------------

   function Case_Lowercase (S : UString) return UString is
     (S.Translate (Ada.Strings.Maps.Constants.Lower_Case_Map));

   -----------------
   -- To_Capitals --
   -----------------

   function To_Capitals (S : UString; First_Also : Boolean) return UString is
      Capitalize : Boolean := First_Also;
   begin
      return Aux : UString := S do
         for I in 1 .. Aux.Length loop
            if Is_Blank (Aux.Element (I)) then
               Capitalize := True;
            else
               if Capitalize then
                  Aux.Replace_Element (I, CHAR.To_Upper (Aux.Element (I)));
               else
                  Aux.Replace_Element (I, CHAR.To_Lower (Aux.Element (I)));
               end if;
               Capitalize := False;
            end if;
         end loop;
      end return;
   end To_Capitals;

   -------------------
   -- Case_Capitals --
   -------------------

   function Case_Capitals (S : UString) return UString is
     (To_Capitals (S, True));

   --------------------
   -- Case_Uppercase --
   --------------------

   function Case_Uppercase (S : UString) return UString is
     (S.Translate (Ada.Strings.Maps.Constants.Upper_Case_Map));

   -------------
   -- Padding --
   -------------

   function Padding
     (Value : UString; Dir : Direction; Char : Character; Length : Natural)
      return UString
   is
   begin
      if Length <= Value.Length then
         return Value;
      else
         declare
            Pad : constant UString := (Length - Value.Length) * Char;
         begin
            if Dir = left then
               return Pad & Value;
            else
               return Value & Pad;
            end if;
         end;
      end if;
   end Padding;

   --------------
   -- Truncate --
   --------------

   function Truncate (S : UString; Length : Natural) return UString is
   begin
      if Length <= S.Length then
         return S.Unbounded_Slice (1, Length);
      else
         return S;
      end if;
   end Truncate;

   ---------------------
   -- Style_Flat_Case --
   ---------------------

   function Style_Flat_Case (S : UString) return UString is
     (Delete_All (Case_Lowercase (Trim_All (S)), ' '));

   ----------------------------
   -- Style_Lower_Camel_Case --
   ----------------------------

   function Style_Lower_Camel_Case (S : UString) return UString is
     (Delete_All (To_Capitals (Trim_All (S), False), ' '));

   ----------------------------
   -- Style_Upper_Camel_Case --
   ----------------------------

   function Style_Upper_Camel_Case (S : UString) return UString is
     (Delete_All (To_Capitals (Trim_All (S), True), ' '));

   ---------------------
   -- Style_Uppercase --
   ---------------------

   function Style_Uppercase (S : UString) return UString is
     (Delete_All (Case_Uppercase (Trim_All (S)), ' '));

   ----------------------
   -- Style_Snake_Case --
   ----------------------

   function Style_Snake_Case (S : UString) return UString is
     (Replace_All (Case_Lowercase (Trim_All (S)), ' ', '_'));

   ----------------------------
   -- Style_Camel_Snake_Case --
   ----------------------------

   function Style_Camel_Snake_Case (S : UString) return UString is
     (Replace_All (To_Capitals (Trim_All (S), False), ' ', '_'));

   ----------------------
   -- Style_Title_Case --
   ----------------------

   function Style_Title_Case (S : UString) return UString is
     (Replace_All (To_Capitals (Trim_All (S), True), ' ', '_'));

   --------------------
   -- Style_All_Caps --
   --------------------

   function Style_All_Caps (S : UString) return UString is
     (Replace_All (Case_Uppercase (Trim_All (S)), ' ', '_'));

   ---------------------
   -- Style_Dash_Case --
   ---------------------

   function Style_Dash_Case (S : UString) return UString is
     (Replace_All (Case_Lowercase (Trim_All (S)), ' ', '-'));

   ----------------------
   -- Style_Train_Case --
   ----------------------

   function Style_Train_Case (S : UString) return UString is
     (Replace_All (To_Capitals (Trim_All (S), True), ' ', '-'));

   ---------------------------
   -- Style_Train_Uppercase --
   ---------------------------

   function Style_Train_Uppercase (S : UString) return UString is
     (Replace_All (Case_Uppercase (Trim_All (S)), ' ', '-'));

begin

   --
   Text_Filter_Matcher.Compile ("^(/[a-zA-Z0-9][^/]*)(.*)$");
   --                             |         1       || 2|
   --                             '-----------------''--'
   --  (1) --> First text filter
   --  (2) --> Empty or additional text filters

end Text_Filters;
