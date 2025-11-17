with Ada.Strings.Unbounded;

package Values is

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

end Values;
