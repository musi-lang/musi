-- Package management and resolution
with Ada.Strings.Unbounded;

package Musi.Packages is

   use Ada.Strings.Unbounded;

   -- Exception types
   Package_Error    : exception;
   Resolution_Error : exception;
   Registry_Error   : exception;

   -- Package information
   type Package_Info is tagged record
      Name        : Unbounded_String;
      Version     : Unbounded_String;
      Main_File   : Unbounded_String;
      Description : Unbounded_String;
   end record;

   -- Package resolution
   function Resolve_Package (Package_Name : String) return String;
   function Find_Package_Root (Start_Path : String) return String;

   -- Package metadata
   function Load_Package_Info (Path : String) return Package_Info;
   procedure Save_Package_Info (Info : Package_Info; Path : String);
   function Is_Valid_Package (Info : Package_Info) return Boolean;

   -- Registry operations
   function Is_Standard_Package (Package_Name : String) return Boolean;
   function Get_Standard_Path (Package_Name : String) return String;

end Musi.Packages;
