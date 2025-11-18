with Musi.CLI;

procedure Main is
   Args : constant Musi.CLI.CLI_Arguments := Musi.CLI.Parse_Arguments;
begin
   Musi.CLI.Execute_Command (Args);
end Main;
