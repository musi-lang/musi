package body Musi.Bytecode.Ops is

   function Get_Opcode_Name (Opcode : Stream_Element) return String is
   begin
      case Opcode is
         when OP_LDSTR  =>
            return "ldstr";

         when OP_CALL   =>
            return "call";

         when OP_RET    =>
            return "ret";

         when OP_LDC_I4 =>
            return "ldc.i4";

         when OP_LDLOC  =>
            return "ldloc";

         when others    =>
            return "unknown";
      end case;
   end Get_Opcode_Name;

end Musi.Bytecode.Ops;
