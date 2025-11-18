-- Command line interface for Musi runtime
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Musi.CLI is

   use Ada.Strings.Unbounded;

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

   procedure Show_Help;
   procedure Show_Version;
   procedure Handle_Run (Args : CLI_Arguments);
   procedure Handle_Exec (Args : CLI_Arguments);
   procedure Handle_Compile (Args : CLI_Arguments);
   procedure Handle_Disasm (Args : CLI_Arguments);
   procedure Handle_Test (Args : CLI_Arguments);
   procedure Handle_Repl (Args : CLI_Arguments);
   procedure Handle_Init (Args : CLI_Arguments);

end Musi.CLI;
