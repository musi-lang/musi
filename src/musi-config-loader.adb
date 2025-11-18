with Ada.Text_IO;
with Ada.Directories;

package body Musi.Config.Loader is

   use Ada.Text_IO;
   use Ada.Directories;

   function Parse_Config_File (Path : String) return Config is
      JSON_Content : constant String := Read_Config_File (Path);
   begin
      return Parse_Config_JSON (JSON_Content);
   end Parse_Config_File;

   function Parse_Config_JSON (JSON_Content : String) return Config is
      Result : Config := Create_Default_Config;
   begin
      -- TODO: Implement JSON parsing using schema
      return Result;
   end Parse_Config_JSON;

   function Read_Config_File (Path : String) return String is
      File    : File_Type;
      Content : Unbounded_String;
   begin
      if not Exists (Path) then
         raise Config_Error with "Config file not found: " & Path;
      end if;

      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Append (Content, Get_Line (File) & ASCII.LF);
      end loop;
      Close (File);

      return To_String (Content);
   end Read_Config_File;

   procedure Write_Config_File (Config : Musi.Config.Config; Path : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      -- TODO: Implement JSON serialization
      Put_Line (File, "{");
      Put_Line (File, "  ""compilerOptions"": {");
      Put_Line
        (File,
         "    ""target"": """
         & Target_To_String (Config.Compiler_Options.Target)
         & """,");
      Put_Line
        (File,
         "    ""outDir"": """
         & To_String (Config.Compiler_Options.Out_Dir)
         & """,");
      Put_Line
        (File,
         "    ""rootDir"": """
         & To_String (Config.Compiler_Options.Root_Dir)
         & """");
      Put_Line (File, "  }");
      Put_Line (File, "}");
      Close (File);
   end Write_Config_File;

   function Validate_Config_Schema (JSON_Content : String) return Boolean is
   begin
      -- TODO: Implement schema validation against msconfig-file.v1.json
      return True;
   end Validate_Config_Schema;

end Musi.Config.Loader;
