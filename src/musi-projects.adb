with Ada.Directories;
with Ada.Strings.Fixed;
with Musi.Projects.Init;

package body Musi.Projects is

   use Ada.Directories;
   use Ada.Strings.Fixed;

   function Find_Project_Root (Start_Path : String) return String is
      Current_Dir : String := Start_Path;
   begin
      loop
         if Exists (Compose (Current_Dir, "msconfig.json"))
           or Exists (Compose (Current_Dir, "mspackage.json"))
         then
            return Current_Dir;
         end if;

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

      raise Not_Found_Error with "No project root found in directory tree";
   end Find_Project_Root;

   function Find_Workspace_Root (Start_Path : String) return String is
      Current_Dir : String := Start_Path;
   begin
      loop
         declare
            Manifest_Path : constant String :=
              Compose (Current_Dir, "mspackage.json");
         begin
            if Exists (Manifest_Path) then
               -- TODO: Parse JSON to check for workspaces field
               return Current_Dir;
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

      raise Not_Found_Error with "No workspace root found in directory tree";
   end Find_Workspace_Root;

   function Load_Project (Project_Root : String) return Project is
      Proj : Project;
   begin
      Proj.Root_Dir := To_Unbounded_String (Project_Root);

      declare
         Config_Path : constant String :=
           Compose (Project_Root, "msconfig.json");
      begin
         if Exists (Config_Path) then
            Proj.Config := Musi.Config.Load_Config (Config_Path);
         else
            Proj.Config := Musi.Config.Create_Default_Config;
         end if;
      end;

      declare
         Manifest_Path : constant String :=
           Compose (Project_Root, "mspackage.json");
      begin
         if Exists (Manifest_Path) then
            Proj.Manifest :=
              Musi.Packages.Load_Package_Manifest (Manifest_Path);
            Proj.Name := Proj.Manifest.Name;
         else
            Proj.Manifest := Musi.Packages.Create_Default_Manifest;
            Proj.Name := To_Unbounded_String ("unnamed-project");
         end if;
      end;

      return Proj;
   end Load_Project;

   procedure Save_Project (Proj : Project) is
      Root : constant String := To_String (Proj.Root_Dir);
   begin
      Musi.Config.Save_Config (Proj.Config, Compose (Root, "msconfig.json"));
      Musi.Packages.Save_Package_Manifest
        (Proj.Manifest, Compose (Root, "mspackage.json"));
   end Save_Project;

   function Is_Valid_Project (Proj : Project) return Boolean is
   begin
      return
        Length (Proj.Name) > 0
        and Length (Proj.Root_Dir) > 0
        and Exists (To_String (Proj.Root_Dir));
   end Is_Valid_Project;

   function Load_Workspace (Workspace_Root : String) return Workspace is
      WS : Workspace;
   begin
      WS.Root_Dir := To_Unbounded_String (Workspace_Root);
      -- TODO: Load workspace projects from manifest
      return WS;
   end Load_Workspace;

   procedure Save_Workspace (WS : Workspace) is
   begin
      -- TODO: Save workspace manifest
      null;
   end Save_Workspace;

   function Is_Valid_Workspace (WS : Workspace) return Boolean is
   begin
      return Length (WS.Root_Dir) > 0 and Exists (To_String (WS.Root_Dir));
   end Is_Valid_Workspace;

   procedure Init_Project
     (Path : String; Name : String; Proj_Type : Project_Type) is
   begin
      case Proj_Type is
         when Executable =>
            Init.Create_Executable_Template (Path, Name);

         when Library    =>
            Init.Create_Library_Template (Path, Name);

         when Package_   =>
            Init.Create_Package_Template (Path, Name);
      end case;
   end Init_Project;

   procedure Init_Workspace (Path : String; Name : String) is
   begin
      Init.Create_Workspace_Template (Path, Name);
   end Init_Workspace;

   procedure Add_Dependency
     (Proj : in out Project; Package_Name : String; Version : String) is
   begin
      Proj.Manifest.Dependencies.Insert
        (To_Unbounded_String (Package_Name), To_Unbounded_String (Version));
   end Add_Dependency;

   procedure Remove_Dependency (Proj : in out Project; Package_Name : String)
   is
   begin
      Proj.Manifest.Dependencies.Delete (To_Unbounded_String (Package_Name));
   end Remove_Dependency;

   function Resolve_Dependencies (Proj : Project) return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;
   begin
      -- TODO: Implement dependency resolution
      return Result;
   end Resolve_Dependencies;

   function Get_Source_Files (Proj : Project) return String_Vectors.Vector is
      Result  : String_Vectors.Vector;
      Src_Dir : constant String := Compose (To_String (Proj.Root_Dir), "src");
   begin
      -- TODO: Scan source directory for .ms files
      if Exists (Src_Dir) then
         Result.Append (To_Unbounded_String (Compose (Src_Dir, "main.ms")));
      end if;
      return Result;
   end Get_Source_Files;

   function Get_Output_Path (Proj : Project) return String is
   begin
      return
        Compose
          (To_String (Proj.Root_Dir),
           To_String (Proj.Config.Compiler_Options.Out_Dir));
   end Get_Output_Path;

   function Get_Target_Name (Proj : Project) return String is
   begin
      case Proj.Project_Type is
         when Executable =>
            return To_String (Proj.Name);

         when Library    =>
            return "lib" & To_String (Proj.Name) & ".a";

         when Package_   =>
            return To_String (Proj.Name) & ".msp";
      end case;
   end Get_Target_Name;

end Musi.Projects;
