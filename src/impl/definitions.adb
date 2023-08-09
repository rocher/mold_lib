package body Definitions is

   procedure Read_Variables (S : String) is null;

   function GNAT_Bug (Key : String) return Boolean is
   begin
      case Key is
         when "mold-rename-source" =>
            null;

         when others =>
            null;
      end case;
      return True;
   end GNAT_Bug;

end Definitions;
