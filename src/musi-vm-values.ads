with Ada.Strings.Unbounded;

package Musi.VM.Values is

   use Ada.Strings.Unbounded;

   type Value_Kind is (Int, Str, Bool, Unit);

   type Value (Kind : Value_Kind := Unit) is record
      case Kind is
         when Int =>
            Int_Value : Integer;

         when Str =>
            Str_Value : Unbounded_String;

         when Bool =>
            Bool_Value : Boolean;

         when Unit =>
            null;
      end case;
   end record;

   type Value_Array is array (Positive range <>) of Value;

   function Make_Int (Val : Integer) return Value;
   function Make_Str (Val : String) return Value;
   function Make_Bool (Val : Boolean) return Value;
   function Make_Unit return Value;

   function To_String (Val : Value) return String;
   function Is_Truthy (Val : Value) return Boolean;

end Musi.VM.Values;
