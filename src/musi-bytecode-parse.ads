with Ada.Streams;
with Ada.Strings.Unbounded;
with Interfaces;

package Musi.Bytecode.Parse is

   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Interfaces;

   function Parse_File (Data : Stream_Element_Array) return Bytecode_File;
   function Extract_Code
     (File : Bytecode_File; Data : Stream_Element_Array)
      return Stream_Element_Array;
   function Extract_Strings
     (File : Bytecode_File; Data : Stream_Element_Array)
      return String_Array_Access;

end Musi.Bytecode.Parse;
