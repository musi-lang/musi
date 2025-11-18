with Ada.Text_IO;
with Musi.Bytecode.Ops;

package body Musi.Bytecode.Disasm is

   use Ada.Text_IO;
   use Musi.Bytecode.Ops;

   procedure Disassemble_Code (Bytecode : Stream_Element_Array) is
      IP : Natural := 0;
   begin
      Put_Line (".fn main()");
      Put_Line ("{");
      while IP < Bytecode'Length loop
         declare
            Opcode : constant Stream_Element :=
              Bytecode (Bytecode'First + Stream_Element_Offset (IP));
         begin
            Print_Instruction (IP, Opcode);
            IP := IP + 1;
         end;
      end loop;
      Put_Line ("}");
   end Disassemble_Code;

   procedure Print_Instruction (IP : Natural; Opcode : Stream_Element) is
      Hex_IP : constant String :=
        "IL_" & Natural'Image (IP) (2 .. Natural'Image (IP)'Last);
   begin
      Put ("  " & Hex_IP & ": ");
      Put_Line (Get_Opcode_Name (Opcode));
   end Print_Instruction;

end Musi.Bytecode.Disasm;
