with Ada.Command_Line;
with Ada.Text_IO;

procedure Musi is
   use Ada.Command_Line;
   use Ada.Text_IO;

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
   begin
      Put_Line ("TODO: Run " & File_Name);
   end Handle_Run;

   procedure Handle_Exec (File_Name : String) is
   begin
      Put_Line ("TODO: Execute " & File_Name);
   end Handle_Exec;

   procedure Handle_Compile (File_Name : String) is
   begin
      Put_Line ("TODO: Compile " & File_Name);
   end Handle_Compile;

   procedure Handle_Disasm (File_Name : String) is
   begin
      Put_Line ("TODO: Disassemble " & File_Name);
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
            Put_Line ("Error: no input files");
         else
            Handle_Run (Argument (2));
         end if;
      elsif Command = "exec" then
         if Argument_Count < 2 then
            Put_Line ("Error: no input files");
         else
            Handle_Exec (Argument (2));
         end if;
      elsif Command = "compile" then
         if Argument_Count < 2 then
            Put_Line ("Error: no input files");
         else
            Handle_Compile (Argument (2));
         end if;
      elsif Command = "disasm" then
         if Argument_Count < 2 then
            Put_Line ("Error: no input files");
         else
            Handle_Disasm (Argument (2));
         end if;
      elsif Command = "test" then
         Handle_Test;
      elsif Command = "repl" then
         Handle_Repl;
      else
         Put_Line ("Error: unknown command '" & Command & "'");
      end if;
   end;
end Musi;
