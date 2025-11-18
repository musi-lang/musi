-- msconfig.json configuration handling
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Musi.Config is

   use Ada.Strings.Unbounded;

   package String_Vectors is new
     Ada.Containers.Vectors (Positive, Unbounded_String);
   package Path_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Unbounded_String,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   type Target_Type is (MS2025, Native, WASM, Embedded);

   type Compiler_Options is record
      -- Core Options
      Target   : Target_Type := MS2025;
      Lib      : String_Vectors.Vector;
      Out_Dir  : Unbounded_String := To_Unbounded_String ("./dist");
      Root_Dir : Unbounded_String := To_Unbounded_String ("./src");
      Base_Url : Unbounded_String := To_Unbounded_String ("./");

      -- Strictness Options (strict=false by default)
      Strict                        : Boolean := False;
      No_Implicit_Any               : Boolean := False;
      No_Implicit_Unit              : Boolean := False;
      Strict_Function_Types         : Boolean := False;
      Strict_Optional_Checks        : Boolean := False;
      No_Missing_Cases_In_Match     : Boolean := False;
      Exact_Optional_Property_Types : Boolean := False;

      -- Code Quality Options
      No_Unused_Locals       : Boolean := False;
      No_Unused_Parameters   : Boolean := False;
      No_Implicit_Returns    : Boolean := False;
      Allow_Unreachable_Code : Boolean := False;
      Allow_Unused_Labels    : Boolean := False;

      -- Output Options
      No_Emit          : Boolean := False;
      No_Emit_On_Error : Boolean := True;
      Remove_Comments  : Boolean := True;

      -- Module Options
      Paths : Path_Maps.Map;

      -- File System Options
      Force_Consistent_Casing    : Boolean := True;
      Allow_Arbitrary_Extensions : Boolean := True;
      Skip_Lib_Check             : Boolean := True;

      -- Project Options
      Composite : Boolean := False;
   end record;

   type Project_Reference is record
      Path : Unbounded_String;
   end record;

   package Reference_Vectors is new
     Ada.Containers.Vectors (Positive, Project_Reference);

   type Config is record
      Compiler_Options : Musi.Config.Compiler_Options;
      Include          : String_Vectors.Vector;
      Exclude          : String_Vectors.Vector;
      Extends          : Unbounded_String;
      Compile_On_Save  : Boolean := False;
      References       : Reference_Vectors.Vector;
   end record;

   function Load_Config (Path : String) return Config;
   function Find_Config (Start_Path : String) return String;
   function Create_Default_Config return Config;
   procedure Save_Config (Config : Musi.Config.Config; Path : String);
   function Target_To_String (Target : Target_Type) return String;
   function String_To_Target (Target : String) return Target_Type;

   -- Strict mode includes:
   -- - No_Implicit_Any
   -- - No_Implicit_Unit
   -- - Strict_Function_Types
   -- - Strict_Optional_Checks
   -- - No_Missing_Cases_In_Match
   -- - Exact_Optional_Property_Types
   -- - No_Unused_Locals
   -- - No_Unused_Parameters
   -- - No_Implicit_Returns
   procedure Apply_Strict_Mode (Options : in out Compiler_Options);
   procedure Apply_Loose_Mode (Options : in out Compiler_Options);


end Musi.Config;
