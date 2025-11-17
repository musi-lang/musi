with Ada.Unchecked_Conversion;

package body Bytecode is

   subtype Four_Bytes is Ada.Streams.Stream_Element_Array (1 .. 4);
   function To_U32 is new Ada.Unchecked_Conversion (Four_Bytes, Unsigned_32);

   function Parse (Data : Stream_Element_Array) return Bytecode_File is
      File : Bytecode_File;
   begin
      if Data'Length < 32 then
         raise Constraint_Error with "invalid bytecode file";
      end if;

      for I in 1 .. 4 loop
         File.Magic (I) :=
           Character'Val (Data (Data'First + Stream_Element_Offset (I - 1)));
      end loop;
      if File.Magic /= "MUSI" then
         raise Constraint_Error with "invalid magic";
      end if;

      File.Version :=
        To_U32 (Four_Bytes (Data (Data'First + 4 .. Data'First + 7)));
      File.Import_Offset :=
        To_U32 (Four_Bytes (Data (Data'First + 8 .. Data'First + 11)));
      File.Import_Size :=
        To_U32 (Four_Bytes (Data (Data'First + 12 .. Data'First + 15)));
      File.Const_Offset :=
        To_U32 (Four_Bytes (Data (Data'First + 16 .. Data'First + 19)));
      File.Const_Size :=
        To_U32 (Four_Bytes (Data (Data'First + 20 .. Data'First + 23)));
      File.Fn_Offset :=
        To_U32 (Four_Bytes (Data (Data'First + 24 .. Data'First + 27)));
      File.Fn_Size :=
        To_U32 (Four_Bytes (Data (Data'First + 28 .. Data'First + 31)));

      return File;
   end Parse;

   function Get_Code
     (File : Bytecode_File; Data : Stream_Element_Array)
      return Stream_Element_Array
   is
      Start : constant Natural := Natural (File.Fn_Offset + File.Fn_Size);
   begin
      if Start >= Data'Length then
         return [1 .. 0 => <>];
      end if;
      return Data (Data'First + Stream_Element_Offset (Start) .. Data'Last);
   end Get_Code;

   function Parse_Strings
     (File : Bytecode_File; Data : Stream_Element_Array)
      return String_Array_Access
   is
      Pos          : Natural := Natural (File.Const_Offset);
      End_Pos      : constant Natural :=
        Natural (File.Const_Offset + File.Const_Size);
      Count        : Unsigned_32;
      Strings      : String_Array_Access;
      String_Count : Natural := 0;
   begin
      if Pos + 4 > End_Pos then
         return new String_Array (1 .. 0);
      end if;

      Count :=
        To_U32
          (Four_Bytes
             (Data
                (Data'First
                 + Stream_Element_Offset (Pos)
                 .. Data'First + Stream_Element_Offset (Pos + 3))));
      Pos := Pos + 4;

      Strings := new String_Array (1 .. Natural (Count));

      for I in 1 .. Natural (Count) loop
         exit when Pos >= End_Pos;

         declare
            Const_Type : constant Stream_Element :=
              Data (Data'First + Stream_Element_Offset (Pos));
         begin
            Pos := Pos + 1;

            if Const_Type = 16#03# then
               -- ConstStr
               declare
                  Str : Unbounded_String;
               begin
                  while Pos < Data'Length
                    and then Data (Data'First + Stream_Element_Offset (Pos))
                             /= 0
                  loop
                     Append
                       (Str,
                        Character'Val
                          (Data (Data'First + Stream_Element_Offset (Pos))));
                     Pos := Pos + 1;
                  end loop;

                  if Pos < Data'Length then
                     String_Count := String_Count + 1;
                     if String_Count <= Strings'Last then
                        Strings (String_Count) := Str;
                     end if;
                     Pos := Pos + 1;
                  end if;
               end;
            else
               case Const_Type is
                  when 16#01#          =>
                     Pos := Pos + 8;

                  when 16#02#          =>
                     Pos := Pos + 8;

                  when 16#04# | 16#05# =>
                     null;

                  when 16#00#          =>
                     null;

                  when others          =>
                     exit;
               end case;
            end if;
         end;
      end loop;

      return Strings;
   end Parse_Strings;

end Bytecode;
