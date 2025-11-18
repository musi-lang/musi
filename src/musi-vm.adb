with Ada.Text_IO;
with Musi.Bytecode.Ops;
with Musi.VM.Opcodes;

package body Musi.VM is

   use Ada.Text_IO;
   use Musi.Bytecode.Ops;
   use Musi.VM.Opcodes;

   procedure Init
     (VM       : in out VM_Instance;
      Bytecode : Stream_Element_Array;
      Strings  : Musi.Bytecode.String_Array_Access) is
   begin
      VM.Stack.Clear;
      VM.Bytecode := new Stream_Element_Array'(Bytecode);
      VM.Strings := Strings;
      VM.Builtins.Insert (To_Unbounded_String ("write"), Builtin_Write'Access);
      VM.Builtins.Insert
        (To_Unbounded_String ("writeln"), Builtin_Writeln'Access);
      VM.IP := 0;
      VM.Halted := False;
   end Init;

   procedure Deinit (VM : in out VM_Instance) is
   begin
      VM.Stack.Clear;
      VM.Builtins.Clear;
   end Deinit;

   procedure Execute (VM : in out VM_Instance) is
      Opcode : Stream_Element;
   begin
      while VM.IP < VM.Bytecode'Length and not VM.Halted loop
         Opcode :=
           VM.Bytecode (VM.Bytecode'First + Stream_Element_Offset (VM.IP));
         VM.IP := VM.IP + 1;

         case Opcode is
            when OP_LDSTR  =>
               Handle_Ldstr (VM);

            when OP_CALL   =>
               Handle_Call (VM);

            when OP_RET    =>
               VM.Halted := True;

            when OP_LDC_I4 =>
               Handle_Ldc_I4 (VM);

            when OP_LDLOC  =>
               Handle_Ldloc (VM);

            when others    =>
               raise Execution_Error
                 with "Unknown opcode: " & Stream_Element'Image (Opcode);
         end case;
      end loop;
   end Execute;

   procedure Push (VM : in out VM_Instance; Val : Value) is
   begin
      VM.Stack.Append (Val);
   end Push;

   function Pop (VM : in out VM_Instance) return Value is
   begin
      if VM.Stack.Is_Empty then
         raise Stack_Underflow with "Cannot pop from empty stack";
      end if;

      declare
         Val : constant Value := VM.Stack.Last_Element;
      begin
         VM.Stack.Delete_Last;
         return Val;
      end;
   end Pop;

   function Stack_Size (VM : VM_Instance) return Natural is
   begin
      return Natural (VM.Stack.Length);
   end Stack_Size;

   function Is_Stack_Empty (VM : VM_Instance) return Boolean is
   begin
      return VM.Stack.Is_Empty;
   end Is_Stack_Empty;

   procedure Reset (VM : in out VM_Instance) is
   begin
      VM.Stack.Clear;
      VM.IP := 0;
      VM.Halted := False;
   end Reset;

   function Is_Halted (VM : VM_Instance) return Boolean is
   begin
      return VM.Halted;
   end Is_Halted;

   procedure Halt (VM : in out VM_Instance) is
   begin
      VM.Halted := True;
   end Halt;

   procedure Dump_Stack (VM : VM_Instance) is
   begin
      Put_Line ("Stack (" & Natural'Image (Stack_Size (VM)) & " items):");
      for I in VM.Stack.First_Index .. VM.Stack.Last_Index loop
         Put_Line
           ("  [" & Natural'Image (I) & "] " & To_String (VM.Stack (I)));
      end loop;
   end Dump_Stack;

   function Get_IP (VM : VM_Instance) return Natural is
   begin
      return VM.IP;
   end Get_IP;


end Musi.VM;
