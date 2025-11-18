with Ada.Streams;
with Ada.Strings.Unbounded;
with Interfaces;

package Musi.Bytecode is

   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Interfaces;

   type Bytecode_File is record
      Magic         : String (1 .. 4);
      Version       : Unsigned_32;
      Import_Offset : Unsigned_32;
      Import_Size   : Unsigned_32;
      Const_Offset  : Unsigned_32;
      Const_Size    : Unsigned_32;
      Fn_Offset     : Unsigned_32;
      Fn_Size       : Unsigned_32;
   end record;

   type String_Array is array (Positive range <>) of Unbounded_String;
   type String_Array_Access is access String_Array;
   type Stream_Element_Array_Access is access Stream_Element_Array;

   function Parse (Data : Stream_Element_Array) return Bytecode_File;
   function Get_Code
     (File : Bytecode_File; Data : Stream_Element_Array)
      return Stream_Element_Array;
   function Parse_Strings
     (File : Bytecode_File; Data : Stream_Element_Array)
      return String_Array_Access;
   procedure Disassemble (Bytecode : Stream_Element_Array);

end Musi.Bytecode;
