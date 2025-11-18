with Ada.Text_IO;
with Ada.Directories;

package body Musi.Packages.Loader is

   use Ada.Text_IO;
   use Ada.Directories;

   function Parse_Manifest_File (Path : String) return Package_Manifest is
      JSON_Content : constant String := Read_Manifest_File (Path);
   begin
      return Parse_Manifest_JSON (JSON_Content);
   end Parse_Manifest_File;

   function Parse_Manifest_JSON (JSON_Content : String) return Package_Manifest
   is
      Result : Package_Manifest := Create_Default_Manifest;
   begin
      -- TODO: Implement JSON parsing using schema
      return Result;
   end Parse_Manifest_JSON;

   function Read_Manifest_File (Path : String) return String is
      File    : File_Type;
      Content : Unbounded_String;
   begin
      if not Exists (Path) then
         raise Package_Error with "Package manifest not found: " & Path;
      end if;

      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Append (Content, Get_Line (File) & ASCII.LF);
      end loop;
      Close (File);

      return To_String (Content);
   end Read_Manifest_File;

   procedure Write_Manifest_File (Manifest : Package_Manifest; Path : String)
   is
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      -- TODO: Implement JSON serialization
      Put_Line (File, "{");
      Put_Line (File, "  ""name"": """ & To_String (Manifest.Name) & """,");
      Put_Line
        (File, "  ""version"": """ & To_String (Manifest.Version) & """,");
      Put_Line
        (File,
         "  ""description"": """ & To_String (Manifest.Description) & """,");
      Put_Line (File, "  ""main"": """ & To_String (Manifest.Main) & """");
      Put_Line (File, "}");
      Close (File);
   end Write_Manifest_File;

   function Validate_Manifest_Schema (JSON_Content : String) return Boolean is
   begin
      -- TODO: Implement schema validation against mspackage-file.v1.json
      return True;
   end Validate_Manifest_Schema;

end Musi.Packages.Loader;
