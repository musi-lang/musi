package body Musi.VM.Opcodes is

   procedure Handle_Ldstr (VM : in out VM_Instance) is
      Index : Natural;
   begin
      if VM.IP >= VM.Bytecode'Length then
         raise Execution_Error with "Unexpected end of bytecode in LDSTR";
      end if;

      Index :=
        Natural
          (VM.Bytecode (VM.Bytecode'First + Stream_Element_Offset (VM.IP)));
      VM.IP := VM.IP + 1;

      if Index > 0 and Index <= VM.Strings'Length then
         Push (VM, Make_Str (To_String (VM.Strings (Index))));
      else
         raise Execution_Error with "Invalid string index in LDSTR";
      end if;
   end Handle_Ldstr;

   procedure Handle_Call (VM : in out VM_Instance) is
      Func_Name : constant Value := Pop (VM);
      Result    : Value;
   begin
      if Func_Name.Kind /= Str then
         raise Execution_Error with "Function name must be string";
      end if;

      declare
         Name : constant Unbounded_String := Func_Name.Str_Value;
      begin
         if VM.Builtins.Contains (Name) then
            -- TODO: Handle arguments properly
            Result := VM.Builtins (Name) ((1 => Make_Str ("Hello, World!")));
            Push (VM, Result);
         else
            raise Execution_Error with "Unknown function: " & To_String (Name);
         end if;
      end;
   end Handle_Call;

   procedure Handle_Ldc_I4 (VM : in out VM_Instance) is
      Value : Integer;
   begin
      -- TODO: Read 4-byte integer from bytecode
      Value := 42; -- Placeholder
      Push (VM, Make_Int (Value));
   end Handle_Ldc_I4;

   procedure Handle_Ldloc (VM : in out VM_Instance) is
   begin
      -- TODO: Load local variable
      Push (VM, Make_Unit);
   end Handle_Ldloc;

end Musi.VM.Opcodes;
