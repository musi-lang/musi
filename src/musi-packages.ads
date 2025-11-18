-- mspackage.json & musi.lock management
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Musi.Packages is

   use Ada.Strings.Unbounded;

   Package_Error    : exception;
   Resolution_Error : exception;
   Registry_Error   : exception;

   package String_Vectors is new
     Ada.Containers.Vectors (Positive, Unbounded_String);
   package String_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Unbounded_String,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   type Author_Info is record
      Name  : Unbounded_String;
      Email : Unbounded_String;
      URL   : Unbounded_String;
   end record;

   type Repository_Info is record
      Repo_Type : Unbounded_String := To_Unbounded_String ("git");
      URL       : Unbounded_String;
      Directory : Unbounded_String;
   end record;

   type Task_Info is record
      Description  : Unbounded_String;
      Command      : Unbounded_String;
      Dependencies : String_Vectors.Vector;
   end record;

   package Task_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Task_Info,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   type Fmt_Config is record
      Include      : String_Vectors.Vector;
      Exclude      : String_Vectors.Vector;
      Use_Tabs     : Boolean := False;
      Line_Width   : Natural := 80;
      Indent_Width : Natural := 2;
      Semi_Colons  : Boolean := True;
      Prose_Wrap   : Unbounded_String := To_Unbounded_String ("preserve");
   end record;

   type Lint_Config is record
      Include      : String_Vectors.Vector;
      Exclude      : String_Vectors.Vector;
      Tags         : String_Vectors.Vector;
      Rule_Include : String_Vectors.Vector;
      Rule_Exclude : String_Vectors.Vector;
   end record;

   type Test_Config is record
      Include : String_Vectors.Vector;
      Exclude : String_Vectors.Vector;
   end record;

   type Package_Manifest is record
      Name                  : Unbounded_String;
      Version               : Unbounded_String;
      Description           : Unbounded_String;
      Main                  : Unbounded_String :=
        To_Unbounded_String ("./index.ms");
      Exports               : String_Maps.Map;
      Imports               : String_Maps.Map;
      Dependencies          : String_Maps.Map;
      Dev_Dependencies      : String_Maps.Map;
      Peer_Dependencies     : String_Maps.Map;
      Optional_Dependencies : String_Maps.Map;
      Tasks                 : Task_Maps.Map;
      Bin                   : String_Maps.Map;
      Files                 : String_Vectors.Vector;
      Keywords              : String_Vectors.Vector;
      Author                : Author_Info;
      License               : Unbounded_String;
      Homepage              : Unbounded_String;
      Repository            : Repository_Info;
      Private_Package       : Boolean := False;
      Workspaces            : String_Vectors.Vector;
      Publish_Registry      : Unbounded_String :=
        To_Unbounded_String ("https://msr.musi-lang.org");
      Exclude               : String_Vectors.Vector;
      Fmt                   : Fmt_Config;
      Lint                  : Lint_Config;
      Test                  : Test_Config;
      Bench                 : Test_Config;
      Publish               : Test_Config;
      Vendor                : Boolean := False;
      Lock_File             : Unbounded_String :=
        To_Unbounded_String ("musi.lock");
   end record;

   function Resolve_Package (Package_Name : String) return String;
   function Find_Package_Root (Start_Path : String) return String;

   function Load_Package_Manifest (Path : String) return Package_Manifest;
   procedure Save_Package_Manifest
     (Manifest : Package_Manifest; Path : String);
   function Is_Valid_Package (Manifest : Package_Manifest) return Boolean;
   function Create_Default_Manifest return Package_Manifest;

   function Is_Standard_Package (Package_Name : String) return Boolean;
   function Get_Standard_Path (Package_Name : String) return String;

   function Is_Valid_Package_Name (Name : String) return Boolean;
   function Is_Valid_Version (Version : String) return Boolean;

end Musi.Packages;
