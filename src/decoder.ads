with Ada.Streams;

package Decoder is

   use Ada.Streams;

   procedure Disassemble (Bytecode : Stream_Element_Array);

end Decoder;
