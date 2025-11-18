with Ada.Directories;
with Ada.Text_IO;
with Musi.Config;
with Musi.Packages;

package body Musi.Projects.Init is

   use Ada.Directories;
   use Ada.Text_IO;

   procedure Create_Executable_Template (Path : String; Name : String) is
   begin
      Create_Directory_Structure (Path);
      Create_Main_File (Path, Name, Executable);
      Create_Config_File (Path, Executable);
      Create_Manifest_File (Path, Name, Executable);
   end Create_Executable_Template;

   procedure Create_Library_Template (Path : String; Name : String) is
   begin
      Create_Directory_Structure (Path);
      Create_Main_File (Path, Name, Library);
      Create_Config_File (Path, Library);
      Create_Manifest_File (Path, Name, Library);
   end Create_Library_Template;

   procedure Create_Package_Template (Path : String; Name : String) is
   begin
      Create_Directory_Structure (Path);
      Create_Main_File (Path, Name, Package_);
      Create_Config_File (Path, Package_);
      Create_Manifest_File (Path, Name, Package_);
   end Create_Package_Template;

   procedure Create_Workspace_Template (Path : String; Name : String) is
      File : File_Type;
   begin
      Create_Directory (Path);

      Create (File, Out_File, Compose (Path, "mspackage.json"));
      Put_Line (File, "{");
      Put_Line (File, "  ""name"": """ & Name & """,");
      Put_Line (File, "  ""version"": ""0.1.0"",");
      Put_Line (File, "  ""workspaces"": [""packages/*""]");
      Put_Line (File, "}");
      Close (File);

      Create_Directory (Compose (Path, "packages"));
   end Create_Workspace_Template;

   procedure Create_Main_File
     (Path : String; Name : String; Proj_Type : Project_Type)
   is
      File      : File_Type;
      Main_Path : constant String :=
        Compose (Compose (Path, "src"), "main.ms");
   begin
      Create (File, Out_File, Main_Path);

      case Proj_Type is
         when Executable =>
            Put_Line (File, "import { writeln } from ""@std/io"";");
            Put_Line (File, "");
            Put_Line (File, "writeln(""Hello from " & Name & "!"");");

         when Library    =>
            Put_Line (File, "// " & Name & " library");
            Put_Line (File, "");
            Put_Line (File, "export val version := ""0.1.0"";");

         when Package_   =>
            Put_Line (File, "// " & Name & " package");
            Put_Line (File, "");
            Put_Line (File, "export val name := """ & Name & """;");
            Put_Line (File, "export val version := ""0.1.0"";");
      end case;

      Close (File);
   end Create_Main_File;

   procedure Create_Config_File (Path : String; Proj_Type : Project_Type) is
      Config : Musi.Config.Config := Musi.Config.Create_Default_Config;
   begin
      case Proj_Type is
         when Executable =>
            Config.Compiler_Options.Out_Dir := To_Unbounded_String ("./dist");

         when Library    =>
            Config.Compiler_Options.Out_Dir := To_Unbounded_String ("./lib");

         when Package_   =>
            Config.Compiler_Options.Out_Dir := To_Unbounded_String ("./build");
      end case;

      Musi.Config.Save_Config (Config, Compose (Path, "msconfig.json"));
   end Create_Config_File;

   procedure Create_Manifest_File
     (Path : String; Name : String; Proj_Type : Project_Type)
   is
      Manifest : Musi.Packages.Package_Manifest :=
        Musi.Packages.Create_Default_Manifest;
   begin
      Manifest.Name := To_Unbounded_String (Name);
      Manifest.Description :=
        To_Unbounded_String ("A Musi " & Project_Type'Image (Proj_Type));

      case Proj_Type is
         when Executable         =>
            Manifest.Main := To_Unbounded_String ("./src/main.ms");

         when Library | Package_ =>
            Manifest.Main := To_Unbounded_String ("./src/index.ms");
      end case;

      Musi.Packages.Save_Package_Manifest
        (Manifest, Compose (Path, "mspackage.json"));
   end Create_Manifest_File;

   procedure Create_Directory_Structure (Path : String) is
   begin
      Create_Directory (Path);
      Create_Directory (Compose (Path, "src"));
      Create_Directory (Compose (Path, "tests"));
   end Create_Directory_Structure;

end Musi.Projects.Init;
