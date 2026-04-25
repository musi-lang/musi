use std::cmp::Ordering;
use std::mem;

use music_seam::{Instruction, Opcode, ProcedureId, TypeId};

use crate::{VmIndexSpace, VmStackKind, VmValueKind};

use super::state::{CallFrame, CallFrameList, EffectHandler, EffectHandlerList, StepOutcome};
use super::{
    CompareOp, GcRef, MvmMode, OperandShape, RuntimeCallMode, RuntimeCallShape, RuntimeFusedOp,
    RuntimeInstruction, RuntimeOperand, Value, ValueList, VmError, VmErrorKind, VmResult,
};

use super::Vm;

mod compare;
mod fast;
mod frame;
mod fused;
mod int;
mod invoke;
mod run;
mod sequence_index;
mod shape_error;
