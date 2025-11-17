package Errors is

   type Runtime_Error is
     (Stack_Overflow,
      Stack_Underflow,
      IO_Error,
      Unexpected_Magic,
      Out_Of_Memory);

   function To_String (Error : Runtime_Error) return String;

end Errors;
