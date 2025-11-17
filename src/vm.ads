with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Values;
with Bytecode;
with Builtins;

package VM is

   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Values;
   use Bytecode;
   use Builtins;

   package Value_Vectors is new Ada.Containers.Vectors (Positive, Value);

   type Builtin_Fn is access function (Args : Value_Array) return Value;

   package Builtin_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Builtin_Fn,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   subtype Builtin_Registry is Builtin_Maps.Map;

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type VM_Type is record
      Stack    : Value_Vectors.Vector;
      Bytecode : Stream_Element_Array_Access;
      Strings  : String_Array_Access;
      Builtins : Builtin_Registry;
      IP       : Natural := 0;
   end record;

   procedure Init
     (VM       : in out VM_Type;
      Bytecode : Stream_Element_Array;
      Strings  : String_Array_Access);
   procedure Deinit (VM : in out VM_Type);
   procedure Execute (VM : in out VM_Type);

end VM;
