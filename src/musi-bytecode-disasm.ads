package Musi.Bytecode.Disasm is

   procedure Disassemble_Code (Bytecode : Stream_Element_Array);
   procedure Print_Instruction (IP : Natural; Opcode : Stream_Element);

end Musi.Bytecode.Disasm;
