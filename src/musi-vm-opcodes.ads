package Musi.VM.Opcodes is

   procedure Handle_Ldstr (VM : in out VM_Instance);
   procedure Handle_Call (VM : in out VM_Instance);
   procedure Handle_Ldc_I4 (VM : in out VM_Instance);
   procedure Handle_Ldloc (VM : in out VM_Instance);

end Musi.VM.Opcodes;
