-- Package management implementation
with Ada.Directories;
with Ada.Strings.Fixed;
with Musi.Packages.Loader;

package body Musi.Packages is

   use Ada.Directories;
   use Ada.Strings.Fixed;

   function Resolve_Package (Package_Name : String) return String is
   begin
      if Is_Standard_Package (Package_Name) then
         return Get_Standard_Path (Package_Name);
      end if;

      -- TODO: Handle user packages and registry lookup
      raise Resolution_Error with "Package not found: " & Package_Name;
   end Resolve_Package;

   function Find_Package_Root (Start_Path : String) return String is
      Current_Dir : String := Start_Path;
   begin
      loop
         if Exists (Compose (Current_Dir, "mspackage.json")) then
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

      raise Package_Error with "No 'mspackage.json' found in directory tree";
   end Find_Package_Root;

   function Load_Package_Manifest (Path : String) return Package_Manifest is
   begin
      return Loader.Parse_Manifest_File (Path);
   end Load_Package_Manifest;

   procedure Save_Package_Manifest (Manifest : Package_Manifest; Path : String)
   is
   begin
      Loader.Write_Manifest_File (Manifest, Path);
   end Save_Package_Manifest;

   function Is_Valid_Package (Manifest : Package_Manifest) return Boolean is
   begin
      return
        Length (Manifest.Name) > 0
        and Length (Manifest.Version) > 0
        and Is_Valid_Package_Name (To_String (Manifest.Name))
        and Is_Valid_Version (To_String (Manifest.Version));
   end Is_Valid_Package;

   function Create_Default_Manifest return Package_Manifest is
      Result : Package_Manifest;
   begin
      Result.Name := To_Unbounded_String ("my-package");
      Result.Version := To_Unbounded_String ("0.1.0");
      Result.Description := To_Unbounded_String ("A Musi package");
      return Result;
   end Create_Default_Manifest;

   function Is_Standard_Package (Package_Name : String) return Boolean is
   begin
      return Index (Package_Name, "@std/") = 1;
   end Is_Standard_Package;

   function Get_Standard_Path (Package_Name : String) return String is
      Std_Name    : constant String :=
        Package_Name (6 .. Package_Name'Last); -- Remove "@std/"
      Package_Dir : constant String := "@std-" & Std_Name;
   begin
      -- Map @std/io -> /packages/@std-io/
      return Compose (Compose ("packages", Package_Dir), Std_Name & ".ms");
   end Get_Standard_Path;

   function Is_Valid_Package_Name (Name : String) return Boolean is
   begin
      -- TODO: Implement regex pattern validation
      -- Pattern: ^(@[a-z0-9-~][a-z0-9-._~]*/)?[a-z0-9-~][a-z0-9-._~]*$
      return Name'Length > 0;
   end Is_Valid_Package_Name;

   function Is_Valid_Version (Version : String) return Boolean is
   begin
      -- TODO: Implement semver validation
      -- Pattern: ^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9-]+)?(\+[a-zA-Z0-9-]+)?$
      return Version'Length > 0;
   end Is_Valid_Version;

end Musi.Packages;
