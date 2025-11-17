with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;
with VM;
with Bytecode;
with Decoder;

procedure Musi is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use GNAT.OS_Lib;

   package SIO renames Ada.Streams.Stream_IO;

   type Stream_Element_Array_Access is access Ada.Streams.Stream_Element_Array;

   procedure Handle_Exec (File_Name : String);

   procedure Show_Help is
   begin
      Put_Line ("Usage:");
      Put_Line ("  musi run <file.ms>          Run Musi source file");
      Put_Line ("  musi exec <file.mso>        Execute bytecode file");
      Put_Line ("  musi compile <file.ms>      Compile to bytecode");
      Put_Line ("  musi disasm <file.mso>      Disassemble bytecode file");
      Put_Line ("  musi test [pattern]         Run tests");
      Put_Line ("  musi repl                   Start interactive REPL");
      Put_Line ("  musi --version              Show version");
      Put_Line ("  musi --help                 Show this help");
   end Show_Help;

   procedure Show_Version is
   begin
      Put_Line ("musi 0.1.0");
   end Show_Version;

   procedure Handle_Run (File_Name : String) is
      Args          : constant Argument_List :=
        [new String'("_build/default/bin/compiler/msc.exe"),
         new String'(File_Name)];
      Success       : Boolean;
      Dot_Pos       : Natural;
      Base_Name     : Unbounded_String;
      Bytecode_File : Unbounded_String;
   begin
      Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last),
         Success      => Success);

      if not Success then
         Put_Line ("Error: Compilation failed");
         return;
      end if;

      Dot_Pos :=
        Ada.Strings.Fixed.Index (File_Name, ".", Ada.Strings.Backward);
      if Dot_Pos = 0 then
         Base_Name := To_Unbounded_String (File_Name);
      else
         Base_Name :=
           To_Unbounded_String (File_Name (File_Name'First .. Dot_Pos - 1));
      end if;

      Bytecode_File := Base_Name & ".mso";
      Handle_Exec (To_String (Bytecode_File));
   end Handle_Run;

   procedure Handle_Exec (File_Name : String) is
      File          : SIO.File_Type;
      File_Size     : SIO.Count;
      Bytecode_Data : Stream_Element_Array_Access;
      Last          : Ada.Streams.Stream_Element_Count;
      BC_File       : Bytecode.Bytecode_File;
      Code          : Stream_Element_Array_Access;
      Strings       : Bytecode.String_Array_Access;
      VM_Instance   : VM.VM_Type;
   begin
      SIO.Open (File, SIO.In_File, File_Name);
      File_Size := SIO.Size (File);

      Bytecode_Data :=
        new Ada.Streams.Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Count (File_Size));
      SIO.Read (File, Bytecode_Data.all, Last);
      SIO.Close (File);

      BC_File := Bytecode.Parse (Bytecode_Data.all);
      Code :=
        new Ada.Streams.Stream_Element_Array'
          (Bytecode.Get_Code (BC_File, Bytecode_Data.all));
      Strings := Bytecode.Parse_Strings (BC_File, Bytecode_Data.all);

      VM.Init (VM_Instance, Code.all, Strings);
      VM.Execute (VM_Instance);
      VM.Deinit (VM_Instance);
   exception
      when others =>
         Put_Line ("Error: Failed to execute " & File_Name);
   end Handle_Exec;

   procedure Handle_Compile (File_Name : String) is
      Args    : constant Argument_List :=
        [new String'("_build/default/bin/msc/msc.exe"),
         new String'(File_Name)];
      Success : Boolean;
   begin
      Spawn
        (Program_Name => Args (Args'First).all,
         Args         => Args (Args'First + 1 .. Args'Last),
         Success      => Success);

      if not Success then
         Put_Line ("Error: Compilation failed");
      else
         Put_Line ("Compilation successful");
      end if;
   end Handle_Compile;

   procedure Handle_Disasm (File_Name : String) is
      File          : SIO.File_Type;
      File_Size     : SIO.Count;
      Bytecode_Data : Stream_Element_Array_Access;
      Last          : Ada.Streams.Stream_Element_Count;
      BC_File       : Bytecode.Bytecode_File;
      Code          : Stream_Element_Array_Access;
   begin
      SIO.Open (File, SIO.In_File, File_Name);
      File_Size := SIO.Size (File);

      Bytecode_Data :=
        new Ada.Streams.Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Count (File_Size));
      SIO.Read (File, Bytecode_Data.all, Last);
      SIO.Close (File);

      BC_File := Bytecode.Parse (Bytecode_Data.all);
      Code :=
        new Ada.Streams.Stream_Element_Array'
          (Bytecode.Get_Code (BC_File, Bytecode_Data.all));

      Decoder.Disassemble (Code.all);
   exception
      when others =>
         Put_Line ("Error: Failed to disassemble " & File_Name);
   end Handle_Disasm;

   procedure Handle_Test is
   begin
      Put_Line ("TODO: Run tests");
   end Handle_Test;

   procedure Handle_Repl is
   begin
      Put_Line ("TODO: Start REPL");
   end Handle_Repl;

begin
   if Argument_Count = 0 then
      Show_Help;
      return;
   end if;

   declare
      Command : constant String := Argument (1);
   begin
      if Command = "--help" or Command = "-h" then
         Show_Help;
      elsif Command = "--version" or Command = "-v" then
         Show_Version;
      elsif Command = "run" then
         if Argument_Count < 2 then
            Put_Line ("Error: No input files");
         else
            Handle_Run (Argument (2));
         end if;
      elsif Command = "exec" then
         if Argument_Count < 2 then
            Put_Line ("Error: No input files");
         else
            Handle_Exec (Argument (2));
         end if;
      elsif Command = "compile" then
         if Argument_Count < 2 then
            Put_Line ("Error: No input files");
         else
            Handle_Compile (Argument (2));
         end if;
      elsif Command = "disasm" then
         if Argument_Count < 2 then
            Put_Line ("Error: No input files");
         else
            Handle_Disasm (Argument (2));
         end if;
      elsif Command = "test" then
         Handle_Test;
      elsif Command = "repl" then
         Handle_Repl;
      else
         Put_Line ("Error: Unknown command '" & Command & "'");
      end if;
   end;
end Musi;
