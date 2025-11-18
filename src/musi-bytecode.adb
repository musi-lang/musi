with Musi.Bytecode.Parser;
with Musi.Bytecode.Disasm;

package body Musi.Bytecode is

   function Parse (Data : Stream_Element_Array) return Bytecode_File is
   begin
      return Parser.Parse_Bytecode_File (Data);
   end Parse;

   function Get_Code
     (File : Bytecode_File; Data : Stream_Element_Array)
      return Stream_Element_Array
   is
   begin
      return Parser.Extract_Code (File, Data);
   end Get_Code;

   function Parse_Strings
     (File : Bytecode_File; Data : Stream_Element_Array)
      return String_Array_Access
   is
   begin
      return Parser.Extract_Strings (File, Data);
   end Parse_Strings;

   procedure Disassemble (Bytecode : Stream_Element_Array) is
   begin
      Disasm.Disassemble_Code (Bytecode);
   end Disassemble;

end Musi.Bytecode;
