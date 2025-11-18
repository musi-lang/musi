-- Project and workspace management
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Musi.Config;
with Musi.Packages;

package Musi.Projects is

   use Ada.Strings.Unbounded;

   Project_Error   : exception;
   Config_Error    : exception;
   Not_Found_Error : exception;

   package String_Vectors is new
     Ada.Containers.Vectors (Positive, Unbounded_String);

   type Project_Type is (Executable, Library, Package_);

   -- Project structure
   type Project is record
      Name         : Unbounded_String;
      Root_Dir     : Unbounded_String;
      Project_Type : Musi.Projects.Project_Type := Executable;
      Config       : Musi.Config.Config;
      Manifest     : Musi.Packages.Package_Manifest;
      Dependencies : String_Vectors.Vector;
   end record;

   type Workspace is record
      Root_Dir : Unbounded_String;
      Projects : String_Vectors.Vector;
   end record;

   function Find_Project_Root (Start_Path : String) return String;
   function Find_Workspace_Root (Start_Path : String) return String;

   function Load_Project (Project_Root : String) return Project;
   procedure Save_Project (Proj : Project);
   function Is_Valid_Project (Proj : Project) return Boolean;

   function Load_Workspace (Workspace_Root : String) return Workspace;
   procedure Save_Workspace (WS : Workspace);
   function Is_Valid_Workspace (WS : Workspace) return Boolean;

   procedure Init_Project
     (Path : String; Name : String; Proj_Type : Project_Type);
   procedure Init_Workspace (Path : String; Name : String);

   procedure Add_Dependency
     (Proj : in out Project; Package_Name : String; Version : String);
   procedure Remove_Dependency (Proj : in out Project; Package_Name : String);
   function Resolve_Dependencies (Proj : Project) return String_Vectors.Vector;

   function Get_Source_Files (Proj : Project) return String_Vectors.Vector;
   function Get_Output_Path (Proj : Project) return String;
   function Get_Target_Name (Proj : Project) return String;

end Musi.Projects;
