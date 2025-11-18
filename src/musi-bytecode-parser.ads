
package Musi.Bytecode.Parser is


   function Parse_Bytecode_File (Data : Stream_Element_Array) return Bytecode_File;
   function Extract_Code
     (File : Bytecode_File; Data : Stream_Element_Array)
      return Stream_Element_Array;
   function Extract_Strings
     (File : Bytecode_File; Data : Stream_Element_Array)
      return String_Array_Access;

end Musi.Bytecode.Parser;
