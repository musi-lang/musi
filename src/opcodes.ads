with VM;

package Opcodes is

   OP_LDSTR  : constant := 16#72#;
   OP_CALL   : constant := 16#28#;
   OP_RET    : constant := 16#2A#;
   OP_LDC_I4 : constant := 16#20#;
   OP_LDLOC  : constant := 16#0E#;

   procedure Handle_Ldstr (VM_Instance : in out VM.VM_Type);
   procedure Handle_Call (VM_Instance : in out VM.VM_Type);
   procedure Handle_Ret (VM_Instance : in out VM.VM_Type);
   procedure Handle_Ldc_I4 (VM_Instance : in out VM.VM_Type);
   procedure Handle_Ldloc (VM_Instance : in out VM.VM_Type);

end Opcodes;
