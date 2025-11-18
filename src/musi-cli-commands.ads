with Ada.Strings.Unbounded;

package Musi.CLI.Commands is

   use Ada.Strings.Unbounded;

   procedure Handle_Run (Args : CLI_Arguments);
   procedure Handle_Exec (Args : CLI_Arguments);
   procedure Handle_Compile (Args : CLI_Arguments);
   procedure Handle_Disasm (Args : CLI_Arguments);
   procedure Handle_Test (Args : CLI_Arguments);
   procedure Handle_Repl (Args : CLI_Arguments);
   procedure Handle_Init (Args : CLI_Arguments);

   procedure Show_Help;
   procedure Show_Version;

end Musi.CLI.Commands;
