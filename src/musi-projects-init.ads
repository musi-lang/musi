with Ada.Strings.Unbounded;

package Musi.Projects.Init is

   use Ada.Strings.Unbounded;

   procedure Create_Executable_Template (Path : String; Name : String);
   procedure Create_Library_Template (Path : String; Name : String);
   procedure Create_Package_Template (Path : String; Name : String);
   procedure Create_Workspace_Template (Path : String; Name : String);

   procedure Create_Main_File
     (Path : String; Name : String; Proj_Type : Project_Type);
   procedure Create_Config_File (Path : String; Proj_Type : Project_Type);
   procedure Create_Manifest_File
     (Path : String; Name : String; Proj_Type : Project_Type);
   procedure Create_Directory_Structure (Path : String);

end Musi.Projects.Init;
