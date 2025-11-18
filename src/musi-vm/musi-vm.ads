-- Virtual machine execution engine
with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Finalization;
with Values;
with Musi.Bytecode;

package Musi.VM is

   use Ada.Streams;
   use Values;

   -- Exception types
   VM_Error        : exception;
   Execution_Error : exception;
   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   -- VM state management
   package Value_Vectors is new Ada.Containers.Vectors (Positive, Value);

   type VM_State is new Ada.Finalization.Controlled with record
      Stack  : Value_Vectors.Vector;
      IP     : Natural := 0;
      Halted : Boolean := False;
   end record;

   overriding
   procedure Init (VM : in out VM_State);
   overriding
   procedure Deinit (VM : in out VM_State);

   -- State operations
   procedure Reset (VM : in out VM_State);
   function Is_Halted (VM : VM_State) return Boolean;

   -- Execution operations
   procedure Exec (VM : in out VM_State; Code : Stream_Element_Array);
   procedure Step (VM : in out VM_State; Code : Stream_Element_Array);
   procedure Halt (VM : in out VM_State);

end Musi.VM;
