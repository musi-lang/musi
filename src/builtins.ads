with Values;

package Builtins is

   use Values;

   type Value_Array is array (Positive range <>) of Value;

   function Builtin_Write (Args : Value_Array) return Value;

end Builtins;
