package body Musi.VM.Values is

   function Make_Int (Val : Integer) return Value is
   begin
      return (Kind => Int, Int_Value => Val);
   end Make_Int;

   function Make_Str (Val : String) return Value is
   begin
      return (Kind => Str, Str_Value => To_Unbounded_String (Val));
   end Make_Str;

   function Make_Bool (Val : Boolean) return Value is
   begin
      return (Kind => Bool, Bool_Value => Val);
   end Make_Bool;

   function Make_Unit return Value is
   begin
      return (Kind => Unit);
   end Make_Unit;

   function To_String (Val : Value) return String is
   begin
      case Val.Kind is
         when Int  =>
            return Integer'Image (Val.Int_Value);

         when Str  =>
            return Ada.Strings.Unbounded.To_String (Val.Str_Value);

         when Bool =>
            return Boolean'Image (Val.Bool_Value);

         when Unit =>
            return "()";
      end case;
   end To_String;

   function Is_Truthy (Val : Value) return Boolean is
   begin
      case Val.Kind is
         when Bool =>
            return Val.Bool_Value;

         when Unit =>
            return False;

         when Int  =>
            return Val.Int_Value /= 0;

         when Str  =>
            return Length (Val.Str_Value) > 0;
      end case;
   end Is_Truthy;

end Musi.VM.Values;
