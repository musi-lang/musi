-- VM builtin functions
with Musi.VM.Values;

package Musi.VM.Builtins is

   use Musi.VM.Values;

   -- Builtin function type
   type Builtin_Fn is access function (Args : Value_Array) return Value;

   -- Standard library builtins
   function Builtin_Write (Args : Value_Array) return Value;
   function Builtin_Writeln (Args : Value_Array) return Value;

end Musi.VM.Builtins;