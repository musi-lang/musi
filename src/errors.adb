package body Errors is

   function To_String (Error : Runtime_Error) return String is
   begin
      case Error is
         when Stack_Overflow   =>
            return "Stack overflow";

         when Stack_Underflow  =>
            return "Stack underflow";

         when IO_Error         =>
            return "I/O error";

         when Unexpected_Magic =>
            return "Unexpected magic value";

         when Out_Of_Memory    =>
            return "Out of memory";
      end case;
   end To_String;

end Errors;
