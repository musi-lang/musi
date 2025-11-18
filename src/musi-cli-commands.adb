with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;
with Ada.Streams;
with Musi.Bytecode;

package body Musi.CLI.Commands is

   use Ada.Text_IO;
   use GNAT.OS_Lib;
   use Ada.Streams;

   package SIO renames Ada.Streams.Stream_IO;
   type Stream_Element_Array_Access is access Stream_Element_Array;

   procedure Show_Help is
   begin
      Put_Line ("Usage:");
      Put_Line ("  musi run <file.ms>          Run Musi source file");
      Put_Line ("  musi exec <file.mso>        Execute bytecode file");
      Put_Line ("  musi compile <file.ms>      Compile to bytecode");
      Put_Line ("  musi disasm <file.mso>      Disassemble bytecode file");
      Put_Line ("  musi test [pattern]         Run tests");
      Put_Line ("  musi repl                   Start interactive REPL");
      Put_Line ("  musi init                   Initialize new project");
      Put_Line ("  musi --version              Show version");
      Put_Line ("  musi --help                 Show this help");
   end Show_Help;

   procedure Show_Version is
   begin
      Put_Line ("musi 0.1.0");
   end Show_Version;

   procedure Handle_Run (Args : CLI_Arguments) is
      File_Name     : constant String := To_String (Args.Options.Input_File);
      Compile_Args  : constant Argument_List :=
        [new String'("_build/default/bin/compiler/msc.exe"),
         new String'(File_Name)];
      Success       : Boolean;
      Dot_Pos       : Natural;
      Base_Name     : Unbounded_String;
      Bytecode_File : Unbounded_String;
   begin
      if File_Name = "" then
         Put_Line ("Error: No input file specified");
         return;
      end if;

      -- with package resolution
      Spawn
        (Program_Name => Compile_Args (Compile_Args'First).all,
         Args         =>
           Compile_Args (Compile_Args'First + 1 .. Compile_Args'Last),
         Success      => Success);

      if not Success then
         Put_Line ("Error: Compilation failed");
         return;
      end if;

      -- bytecode
      Dot_Pos :=
        Ada.Strings.Fixed.Index (File_Name, ".", Ada.Strings.Backward);
      if Dot_Pos = 0 then
         Base_Name := To_Unbounded_String (File_Name);
      else
         Base_Name :=
           To_Unbounded_String (File_Name (File_Name'First .. Dot_Pos - 1));
      end if;

      Bytecode_File := Base_Name & ".mso";
      Handle_Exec
        ((Options   =>
            (Command     => Exec,
             Input_File  => Bytecode_File,
             Output_File => <>,
             Verbose     => False,
             Debug       => False),
          Files     => <>,
          Arguments => <>));
   end Handle_Run;

   procedure Handle_Exec (Args : CLI_Arguments) is
      File_Name : constant String := To_String (Args.Options.Input_File);
   begin
      if File_Name = "" then
         Put_Line ("Error: No bytecode file specified");
         return;
      end if;

      -- TODO: with Musi.VM for execution
      Put_Line ("TODO: Execute " & File_Name & " with package-aware VM");
   end Handle_Exec;

   procedure Handle_Compile (Args : CLI_Arguments) is
      File_Name    : constant String := To_String (Args.Options.Input_File);
      Compile_Args : constant Argument_List :=
        [new String'("_build/default/bin/compiler/msc.exe"),
         new String'(File_Name)];
      Success      : Boolean;
   begin
      if File_Name = "" then
         Put_Line ("Error: No input file specified");
         return;
      end if;

      Spawn
        (Program_Name => Compile_Args (Compile_Args'First).all,
         Args         =>
           Compile_Args (Compile_Args'First + 1 .. Compile_Args'Last),
         Success      => Success);

      if not Success then
         Put_Line ("Error: Compilation failed");
      else
         Put_Line ("Compilation successful");
      end if;
   end Handle_Compile;

   procedure Handle_Disasm (Args : CLI_Arguments) is
      File_Name     : constant String := To_String (Args.Options.Input_File);
      File          : SIO.File_Type;
      File_Size     : SIO.Count;
      Bytecode_Data : Stream_Element_Array_Access;
      Last          : Stream_Element_Count;
      BC_File       : Musi.Bytecode.Bytecode_File;
      Code          : Stream_Element_Array_Access;
   begin
      if File_Name = "" then
         Put_Line ("Error: No bytecode file specified");
         return;
      end if;

      SIO.Open (File, SIO.In_File, File_Name);
      File_Size := SIO.Size (File);

      Bytecode_Data :=
        new Stream_Element_Array (1 .. Stream_Element_Count (File_Size));
      SIO.Read (File, Bytecode_Data.all, Last);
      SIO.Close (File);

      BC_File := Musi.Bytecode.Parse (Bytecode_Data.all);
      Code :=
        new Stream_Element_Array'
          (Musi.Bytecode.Get_Code (BC_File, Bytecode_Data.all));

      Musi.Bytecode.Disassemble (Code.all);
   exception
      when others =>
         Put_Line ("Error: Failed to disassemble " & File_Name);
   end Handle_Disasm;

   procedure Handle_Test (Args : CLI_Arguments) is
   begin
      Put_Line ("TODO: Run tests with package resolution");
   end Handle_Test;

   procedure Handle_Repl (Args : CLI_Arguments) is
   begin
      Put_Line ("TODO: Start REPL with package-aware imports");
   end Handle_Repl;

   procedure Handle_Init (Args : CLI_Arguments) is
   begin
      Put_Line ("TODO: Ready new Musi project with mspackage.json");
   end Handle_Init;

end Musi.CLI.Commands;
