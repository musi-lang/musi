-- New main entry point using Musi.CLI
with Musi.CLI;

procedure Musi_Main is
   Args : constant Musi.CLI.CLI_Arguments := Musi.CLI.Parse_Arguments;
begin
   Musi.CLI.Execute_Command (Args);
end Musi_Main;
