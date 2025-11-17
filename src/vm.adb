with Opcodes;

package body VM is

   use Opcodes;

   procedure Init
     (VM       : in out VM_Type;
      Bytecode : Stream_Element_Array;
      Strings  : String_Array_Access) is
   begin
      VM.Stack.Clear;
      VM.Bytecode := new Stream_Element_Array'(Bytecode);
      VM.Strings := Strings;
      VM.Builtins.Insert
        (To_Unbounded_String ("writeln"), Builtin_Write'Access);
      VM.IP := 0;
   end Init;

   procedure Deinit (VM : in out VM_Type) is
   begin
      VM.Stack.Clear;
   end Deinit;

   procedure Execute (VM : in out VM_Type) is
      Opcode : Stream_Element;
   begin
      while VM.IP < VM.Bytecode'Length loop
         Opcode :=
           VM.Bytecode (VM.Bytecode'First + Stream_Element_Offset (VM.IP));
         VM.IP := VM.IP + 1;

         case Opcode is
            when OP_LDSTR  =>
               Handle_Ldstr (VM);

            when OP_CALL   =>
               Handle_Call (VM);

            when OP_RET    =>
               return;

            when OP_LDC_I4 =>
               Handle_Ldc_I4 (VM);

            when OP_LDLOC  =>
               Handle_Ldloc (VM);

            when others    =>
               raise Constraint_Error with "Unknown opcode";
         end case;
      end loop;
   end Execute;

end VM;
