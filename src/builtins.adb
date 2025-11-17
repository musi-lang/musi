with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Builtins is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   function Builtin_Write (Args : Value_Array) return Value is
   begin
      if Args'Length >= 1 and then Args (Args'First).Kind = Str then
         Put_Line (To_String (Args (Args'First).Str_Value));
      end if;
      return (Kind => Unit);
   end Builtin_Write;

end Builtins;
