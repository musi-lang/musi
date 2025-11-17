with Interfaces;
with Values;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Builtins;

package body Opcodes is

   use Interfaces;
   use Values;
   use Ada.Strings.Unbounded;
   use Ada.Streams;

   subtype Four_Bytes is Stream_Element_Array (1 .. 4);
   function To_U32 is new Ada.Unchecked_Conversion (Four_Bytes, Unsigned_32);
   function To_I32 is new Ada.Unchecked_Conversion (Four_Bytes, Integer_32);


   procedure Handle_Ldstr (VM_Instance : in out VM.VM_Type) is
      Bytes : Stream_Element_Array (1 .. 4);
      Idx   : Unsigned_32;
   begin
      Bytes :=
        VM_Instance.Bytecode
          (VM_Instance.Bytecode'First
           + Stream_Element_Offset (VM_Instance.IP)
           .. VM_Instance.Bytecode'First
              + Stream_Element_Offset (VM_Instance.IP + 3));
      Idx := To_U32 (Bytes);
      VM_Instance.IP := VM_Instance.IP + 4;
      if Natural (Idx) + 1 <= VM_Instance.Strings'Last then
         VM_Instance.Stack.Append
           (Value'
              (Kind      => Str,
               Str_Value => VM_Instance.Strings (Natural (Idx) + 1)));
      end if;
   end Handle_Ldstr;

   procedure Handle_Call (VM_Instance : in out VM.VM_Type) is
      Start_Pos : constant Natural := VM_Instance.IP;
      End_Pos   : Natural := Start_Pos;
      Name      : Unbounded_String;
   begin
      while End_Pos < VM_Instance.Bytecode'Length
        and then VM_Instance.Bytecode
                   (VM_Instance.Bytecode'First
                    + Stream_Element_Offset (End_Pos))
                 /= 0
      loop
         End_Pos := End_Pos + 1;
      end loop;

      for I in Start_Pos .. End_Pos - 1 loop
         Ada.Strings.Unbounded.Append
           (Name,
            Character'Val
              (VM_Instance.Bytecode
                 (VM_Instance.Bytecode'First + Stream_Element_Offset (I))));
      end loop;

      VM_Instance.IP := End_Pos + 1;

      if not VM_Instance.Stack.Is_Empty then
         declare
            Arg    : constant Value := VM_Instance.Stack.Last_Element;
            Args   : constant Builtins.Value_Array (1 .. 1) := [1 => Arg];
            Result : Value;
         begin
            VM_Instance.Stack.Delete_Last;
            if VM_Instance.Builtins.Contains (Name) then
               Result := VM_Instance.Builtins.Element (Name) (Args);
               VM_Instance.Stack.Append (Result);
            else
               VM_Instance.Stack.Append (Value'(Kind => Unit));
            end if;
         end;
      end if;
   end Handle_Call;

   procedure Handle_Ret (VM_Instance : in out VM.VM_Type) is
   begin
      null;
   end Handle_Ret;

   procedure Handle_Ldc_I4 (VM_Instance : in out VM.VM_Type) is
      Bytes : Stream_Element_Array (1 .. 4);
      Val   : Integer_32;
   begin
      Bytes :=
        VM_Instance.Bytecode
          (VM_Instance.Bytecode'First
           + Stream_Element_Offset (VM_Instance.IP)
           .. VM_Instance.Bytecode'First
              + Stream_Element_Offset (VM_Instance.IP + 3));
      Val := To_I32 (Bytes);
      VM_Instance.IP := VM_Instance.IP + 4;
      VM_Instance.Stack.Append
        (Value'(Kind => Int, Int_Value => Integer (Val)));
   end Handle_Ldc_I4;

   procedure Handle_Ldloc (VM_Instance : in out VM.VM_Type) is
   begin
      VM_Instance.IP := VM_Instance.IP + 4;
      VM_Instance.Stack.Append (Value'(Kind => Unit));
   end Handle_Ldloc;

end Opcodes;
