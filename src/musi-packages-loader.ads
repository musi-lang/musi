with Ada.Strings.Unbounded;

package Musi.Packages.Loader is

   use Ada.Strings.Unbounded;

   function Parse_Manifest_File (Path : String) return Package_Manifest;
   function Parse_Manifest_JSON
     (JSON_Content : String) return Package_Manifest;

   function Read_Manifest_File (Path : String) return String;
   procedure Write_Manifest_File (Manifest : Package_Manifest; Path : String);

   function Validate_Manifest_Schema (JSON_Content : String) return Boolean;

end Musi.Packages.Loader;
