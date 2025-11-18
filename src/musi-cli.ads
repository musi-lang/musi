with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Musi.Packages;
with Musi.VM;
with Musi.Bytecode;

package Musi.CLI is

   use Ada.Strings.Unbounded;
   use Musi.Packages;

   type Command_Type is
     (Run, Exec, Compile, Disasm, Test, Repl, Init, Help, Version);

   type CLI_Options is record
      Command     : Command_Type;
      Input_File  : Unbounded_String;
      Output_File : Unbounded_String;
      Verbose     : Boolean := False;
      Debug       : Boolean := False;
   end record;

   package String_Vectors is new
     Ada.Containers.Vectors (Positive, Unbounded_String);

   type CLI_Arguments is record
      Options   : CLI_Options;
      Files     : String_Vectors.Vector;
      Arguments : String_Vectors.Vector;
   end record;

   function Parse_Arguments return CLI_Arguments;
   procedure Execute_Command (Args : CLI_Arguments);


end Musi.CLI;
