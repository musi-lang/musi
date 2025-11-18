with Ada.Directories;
with Musi.Config.Loader;

package body Musi.Config is

   use Ada.Directories;

   function Load_Config (Path : String) return Config is
   begin
      return Loader.Parse_Config_File (Path);
   end Load_Config;

   function Find_Config (Start_Path : String) return String is
      Current_Dir : String := Start_Path;
      Config_File : constant String := "msconfig.json";
   begin
      loop
         declare
            Config_Path : constant String :=
              Compose (Current_Dir, Config_File);
         begin
            if Exists (Config_Path) then
               return Config_Path;
            end if;
         end;

         declare
            Parent : constant String := Containing_Directory (Current_Dir);
         begin
            if Parent = Current_Dir then
               -- reached filesystem root
               exit;
            end if;
            Current_Dir := Parent;
         end;
      end loop;

      raise Config_Error with "No 'msconfig.json' found in directory tree";
   end Find_Config;

   function Create_Default_Config return Config is
      Result : Config;
   begin
      Result.Compiler_Options.Lib.Append (To_Unbounded_String ("MS2025"));
      return Result;
   end Create_Default_Config;

   procedure Save_Config (Config : Musi.Config.Config; Path : String) is
   begin
      Loader.Write_Config_File (Config, Path);
   end Save_Config;

   function Target_To_String (Target : Target_Type) return String is
   begin
      case Target is
         when MS2025   =>
            return "MS2025";

         when Native   =>
            return "native";

         when WASM     =>
            return "wasm";

         when Embedded =>
            return "embedded";
      end case;
   end Target_To_String;

   function String_To_Target (Target : String) return Target_Type is
   begin
      if Target = "MS2025" then
         return MS2025;
      elsif Target = "native" then
         return Native;
      elsif Target = "wasm" then
         return WASM;
      elsif Target = "embedded" then
         return Embedded;
      else
         raise Config_Error with "Invalid target: " & Target;
      end if;
   end String_To_Target;

   procedure Apply_Strict_Mode (Options : in out Compiler_Options) is
   begin
      Options.Strict := True;
      Options.No_Implicit_Any := True;
      Options.No_Implicit_Unit := True;
      Options.Strict_Function_Types := True;
      Options.Strict_Optional_Checks := True;
      Options.No_Missing_Cases_In_Match := True;
      Options.Exact_Optional_Property_Types := True;
      Options.No_Unused_Locals := True;
      Options.No_Unused_Parameters := True;
      Options.No_Implicit_Returns := True;
   end Apply_Strict_Mode;

   procedure Apply_Loose_Mode (Options : in out Compiler_Options) is
   begin
      Options.Strict := False;
      Options.No_Implicit_Any := False;
      Options.No_Implicit_Unit := False;
      Options.Strict_Function_Types := False;
      Options.Strict_Optional_Checks := False;
      Options.No_Missing_Cases_In_Match := False;
      Options.Exact_Optional_Property_Types := False;
      Options.No_Unused_Locals := False;
      Options.No_Unused_Parameters := False;
      Options.No_Implicit_Returns := False;
   end Apply_Loose_Mode;

end Musi.Config;
