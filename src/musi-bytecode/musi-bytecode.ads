-- Bytecode file format and operations
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Interfaces;

package Musi.Bytecode is

   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Interfaces;

   type Import_Entry is record
      Module_Name : Unbounded_String;
      Symbol_Name : Unbounded_String;
   end record;

   type Constant_Entry is record
      Value : Unbounded_String;
   end record;

   type Function_Entry is record
      Name      : Unbounded_String;
      Arity     : Natural;
      Code_Size : Natural;
      Code      : Stream_Element_Array_Access;
   end record;

   package Import_Vectors is new
     Ada.Containers.Vectors (Positive, Import_Entry);
   package Constant_Vectors is new
     Ada.Containers.Vectors (Positive, Constant_Entry);
   package Function_Vectors is new
     Ada.Containers.Vectors (Positive, Function_Entry);

   type Bytecode_Module is record
      Magic     : String (1 .. 4) := "MUSI";
      Version   : Unsigned_32 := 1;
      Imports   : Import_Vectors.Vector;
      Constants : Constant_Vectors.Vector;
      Functions : Function_Vectors.Vector;
   end record;

   type Raw_Bytecode_File is record
      Magic         : String (1 .. 4);
      Version       : Unsigned_32;
      Import_Offset : Unsigned_32;
      Import_Size   : Unsigned_32;
      Const_Offset  : Unsigned_32;
      Const_Size    : Unsigned_32;
      Fn_Offset     : Unsigned_32;
      Fn_Size       : Unsigned_32;
   end record;

   type String_Array is array (Positive range <>) of Unbounded_String;
   type String_Array_Access is access String_Array;
   type Stream_Element_Array_Access is access Stream_Element_Array;

   function Load_Module (Data : Stream_Element_Array) return Bytecode_Module;
   procedure Save_Module
     (Module : Bytecode_Module; Data : out Stream_Element_Array);

   function Parse_Raw (Data : Stream_Element_Array) return Raw_Bytecode_File;
   function Raw_To_Module
     (Raw : Raw_Bytecode_File; Data : Stream_Element_Array)
      return Bytecode_Module;
   function Module_To_Raw (Module : Bytecode_Module) return Raw_Bytecode_File;

end Musi.Bytecode;
