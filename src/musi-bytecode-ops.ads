
package Musi.Bytecode.Ops is


   OP_LDSTR  : constant := 16#72#;
   OP_CALL   : constant := 16#28#;
   OP_RET    : constant := 16#2A#;
   OP_LDC_I4 : constant := 16#20#;
   OP_LDLOC  : constant := 16#0E#;


   function Get_Opcode_Name (Opcode : Stream_Element) return String;

end Musi.Bytecode.Ops;
