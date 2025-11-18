-- Project and workspace management
with Ada.Strings.Unbounded;

package Musi.Projects is

   use Ada.Strings.Unbounded;

   -- Exception types
   Project_Error   : exception;
   Config_Error    : exception;
   Not_Found_Error : exception;

   -- Target types
   type Target_Type is (MS2025, Native, WASM, Embedded);

   -- Project configuration
   type Project_Config is tagged record
      Root_Dir   : Unbounded_String;
      Source_Dir : Unbounded_String;
      Output_Dir : Unbounded_String;
      Target     : Target_Type := MS2025;
   end record;

   -- Project discovery
   function Find_Project_Root (Start_Path : String) return String;

   -- Config management
   function Load_Config (Project_Root : String) return Project_Config;
   procedure Save_Config (Config : Project_Config; Project_Root : String);
   function Is_Valid_Config (Config : Project_Config) return Boolean;

   -- Project readying
   procedure Init_Project (Path : String);

end Musi.Projects;
