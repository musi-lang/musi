with Ada.Strings.Unbounded;

package Musi.Config.Loader is

   use Ada.Strings.Unbounded;

   function Parse_Config_File (Path : String) return Config;
   function Parse_Config_JSON (JSON_Content : String) return Config;

   function Read_Config_File (Path : String) return String;
   procedure Write_Config_File (Config : Musi.Config.Config; Path : String);

   function Validate_Config_Schema (JSON_Content : String) return Boolean;

end Musi.Config.Loader;
