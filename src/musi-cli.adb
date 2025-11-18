with Ada.Command_Line;
with Musi.CLI.Commands;

package body Musi.CLI is

   use Ada.Command_Line;

   function Parse_Arguments return CLI_Arguments is
      Args : CLI_Arguments;
   begin
      if Argument_Count = 0 then
         Args.Options.Command := Help;
         return Args;
      end if;

      declare
         Command_Str : constant String := Argument (1);
      begin
         if Command_Str = "run" then
            Args.Options.Command := Run;
         elsif Command_Str = "exec" then
            Args.Options.Command := Exec;
         elsif Command_Str = "compile" then
            Args.Options.Command := Compile;
         elsif Command_Str = "disasm" then
            Args.Options.Command := Disasm;
         elsif Command_Str = "test" then
            Args.Options.Command := Test;
         elsif Command_Str = "repl" then
            Args.Options.Command := Repl;
         elsif Command_Str = "init" then
            Args.Options.Command := Init;
         elsif Command_Str = "--help" or Command_Str = "-h" then
            Args.Options.Command := Help;
         elsif Command_Str = "--version" or Command_Str = "-v" then
            Args.Options.Command := Version;
         else
            Args.Options.Command := Help;
         end if;

         if Argument_Count >= 2 then
            Args.Options.Input_File := To_Unbounded_String (Argument (2));
         end if;
      end;

      return Args;
   end Parse_Arguments;

   procedure Execute_Command (Args : CLI_Arguments) is
   begin
      case Args.Options.Command is
         when Run     =>
            Commands.Handle_Run (Args);

         when Exec    =>
            Commands.Handle_Exec (Args);

         when Compile =>
            Commands.Handle_Compile (Args);

         when Disasm  =>
            Commands.Handle_Disasm (Args);

         when Test    =>
            Commands.Handle_Test (Args);

         when Repl    =>
            Commands.Handle_Repl (Args);

         when Init    =>
            Commands.Handle_Init (Args);

         when Help    =>
            Commands.Show_Help;

         when Version =>
            Commands.Show_Version;
      end case;
   end Execute_Command;

end Musi.CLI;
