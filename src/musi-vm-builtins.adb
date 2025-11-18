with Ada.Text_IO;

package body Musi.VM.Builtins is

   use Ada.Text_IO;

   function Builtin_Write (Args : Value_Array) return Value is
   begin
      for I in Args'Range loop
         Put (To_String (Args (I)));
      end loop;
      return Make_Unit;
   end Builtin_Write;

   function Builtin_Writeln (Args : Value_Array) return Value is
   begin
      for I in Args'Range loop
         Put (To_String (Args (I)));
      end loop;
      New_Line;
      return Make_Unit;
   end Builtin_Writeln;

end Musi.VM.Builtins;
