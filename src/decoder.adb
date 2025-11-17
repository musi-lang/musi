with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Interfaces;
with Ada.Unchecked_Conversion;
with Opcodes;

package body Decoder is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Interfaces;
   use Opcodes;

   subtype Four_Bytes is Ada.Streams.Stream_Element_Array (1 .. 4);
   function To_U32 is new Ada.Unchecked_Conversion (Four_Bytes, Unsigned_32);
   function To_I32 is new Ada.Unchecked_Conversion (Four_Bytes, Integer_32);

   procedure Format_Instr_Header (IP : Natural) is
   begin
      Put ("IL_" & Natural'Image (IP) (2 .. Natural'Image (IP)'Last) & ": ");
   end Format_Instr_Header;

   procedure Decode_Ldstr
     (Bytecode : Stream_Element_Array; IP : in out Natural)
   is
      Bytes : Stream_Element_Array (1 .. 4);
      Idx   : Unsigned_32;
   begin
      if IP + 4 <= Bytecode'Length then
         Bytes :=
           Bytecode
             (Bytecode'First
              + Stream_Element_Offset (IP)
              .. Bytecode'First + Stream_Element_Offset (IP + 3));
         Idx := To_U32 (Four_Bytes (Bytes));
         Put_Line ("ldstr" & Unsigned_32'Image (Idx));
         IP := IP + 4;
      else
         Put_Line ("ldstr (truncated)");
      end if;
   end Decode_Ldstr;

   procedure Decode_Call (Bytecode : Stream_Element_Array; IP : in out Natural)
   is
      Start_Pos : constant Natural := IP;
      End_Pos   : Natural := Start_Pos;
      Name      : Unbounded_String;
   begin
      while End_Pos < Bytecode'Length
        and then Bytecode (Bytecode'First + Stream_Element_Offset (End_Pos))
                 /= 0
      loop
         End_Pos := End_Pos + 1;
      end loop;

      Put ("call ");
      if End_Pos < Bytecode'Length then
         for I in Start_Pos .. End_Pos - 1 loop
            Append
              (Name,
               Character'Val
                 (Bytecode (Bytecode'First + Stream_Element_Offset (I))));
         end loop;
         Put (To_String (Name));
         IP := End_Pos + 1;
      else
         Put ("(truncated)");
      end if;
      New_Line;
   end Decode_Call;

   procedure Decode_Ldc_I4
     (Bytecode : Stream_Element_Array; IP : in out Natural)
   is
      Bytes : Stream_Element_Array (1 .. 4);
      Val   : Integer_32;
   begin
      if IP + 4 <= Bytecode'Length then
         Bytes :=
           Bytecode
             (Bytecode'First
              + Stream_Element_Offset (IP)
              .. Bytecode'First + Stream_Element_Offset (IP + 3));
         Val := To_I32 (Four_Bytes (Bytes));
         Put_Line ("ldc.i4" & Integer_32'Image (Val));
         IP := IP + 4;
      else
         Put_Line ("ldc.i4 (truncated)");
      end if;
   end Decode_Ldc_I4;

   procedure Decode_Ldloc
     (Bytecode : Stream_Element_Array; IP : in out Natural)
   is
      Bytes : Stream_Element_Array (1 .. 4);
      Idx   : Unsigned_32;
   begin
      if IP + 4 <= Bytecode'Length then
         Bytes :=
           Bytecode
             (Bytecode'First
              + Stream_Element_Offset (IP)
              .. Bytecode'First + Stream_Element_Offset (IP + 3));
         Idx := To_U32 (Four_Bytes (Bytes));
         Put_Line ("ldloc" & Unsigned_32'Image (Idx));
         IP := IP + 4;
      else
         Put_Line ("ldloc (truncated)");
      end if;
   end Decode_Ldloc;

   procedure Decode_Ret is
   begin
      Put_Line ("ret");
   end Decode_Ret;

   procedure Decode_Unknown (Opcode : Stream_Element) is
   begin
      Put_Line ("// unknown 0x" & Stream_Element'Image (Opcode));
   end Decode_Unknown;

   procedure Disassemble (Bytecode : Stream_Element_Array) is
      IP     : Natural := 0;
      Opcode : Stream_Element;
   begin
      Put_Line ("// Bytecode disassembly");
      Put_Line ("// Size:" & Natural'Image (Bytecode'Length) & " bytes");

      while IP < Bytecode'Length loop
         Format_Instr_Header (IP);

         Opcode := Bytecode (Bytecode'First + Stream_Element_Offset (IP));
         IP := IP + 1;

         case Opcode is
            when OP_LDSTR  =>
               Decode_Ldstr (Bytecode, IP);

            when OP_CALL   =>
               Decode_Call (Bytecode, IP);

            when OP_RET    =>
               Decode_Ret;

            when OP_LDC_I4 =>
               Decode_Ldc_I4 (Bytecode, IP);

            when OP_LDLOC  =>
               Decode_Ldloc (Bytecode, IP);

            when others    =>
               Decode_Unknown (Opcode);
         end case;
      end loop;
   end Disassemble;

end Decoder;
