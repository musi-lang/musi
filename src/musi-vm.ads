with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Musi.VM.Values;
with Musi.VM.Builtins;
with Musi.Bytecode;

package Musi.VM is

   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Musi.VM.Values;
   use Musi.VM.Builtins;

   VM_Error        : exception;
   Execution_Error : exception;
   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   package Value_Vectors is new Ada.Containers.Vectors (Positive, Value);
   package Builtin_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Builtin_Fn,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=");

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type VM_Instance is record
      Stack    : Value_Vectors.Vector;
      Bytecode : Stream_Element_Array_Access;
      Strings  : Musi.Bytecode.String_Array_Access;
      Builtins : Builtin_Maps.Map;
      IP       : Natural := 0;
      Halted   : Boolean := False;
   end record;

   procedure Init
     (VM       : in out VM_Instance;
      Bytecode : Stream_Element_Array;
      Strings  : Musi.Bytecode.String_Array_Access);
   procedure Deinit (VM : in out VM_Instance);
   procedure Execute (VM : in out VM_Instance);

   procedure Push (VM : in out VM_Instance; Val : Value);
   function Pop (VM : in out VM_Instance) return Value;
   function Stack_Size (VM : VM_Instance) return Natural;
   function Is_Stack_Empty (VM : VM_Instance) return Boolean;

   procedure Reset (VM : in out VM_Instance);
   function Is_Halted (VM : VM_Instance) return Boolean;
   procedure Halt (VM : in out VM_Instance);

   procedure Dump_Stack (VM : VM_Instance);
   function Get_IP (VM : VM_Instance) return Natural;

end Musi.VM;
