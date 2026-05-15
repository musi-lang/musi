use crate::artifact::{ProcedureId, StringId};
use std::str::FromStr;

pub type RootSlotList = Box<[u16]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SafePointKind {
    Call = 0,
    CallIndirect = 1,
    CallForeign = 2,
    Allocation = 3,
    Collection = 4,
    PinEnter = 5,
    PinExit = 6,
    Yield = 7,
    Trap = 8,
}

impl SafePointKind {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Call => "call",
            Self::CallIndirect => "call.ind",
            Self::CallForeign => "call.ffi",
            Self::Allocation => "allocation",
            Self::Collection => "collection",
            Self::PinEnter => "pin.enter",
            Self::PinExit => "pin.exit",
            Self::Yield => "yield",
            Self::Trap => "trap",
        }
    }

    #[must_use]
    pub const fn from_wire(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Call),
            1 => Some(Self::CallIndirect),
            2 => Some(Self::CallForeign),
            3 => Some(Self::Allocation),
            4 => Some(Self::Collection),
            5 => Some(Self::PinEnter),
            6 => Some(Self::PinExit),
            7 => Some(Self::Yield),
            8 => Some(Self::Trap),
            _ => None,
        }
    }

    #[must_use]
    pub const fn wire_code(self) -> u8 {
        match self {
            Self::Call => 0,
            Self::CallIndirect => 1,
            Self::CallForeign => 2,
            Self::Allocation => 3,
            Self::Collection => 4,
            Self::PinEnter => 5,
            Self::PinExit => 6,
            Self::Yield => 7,
            Self::Trap => 8,
        }
    }
}

impl FromStr for SafePointKind {
    type Err = ();

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            "call" => Ok(Self::Call),
            "call.ind" => Ok(Self::CallIndirect),
            "call.ffi" => Ok(Self::CallForeign),
            "allocation" => Ok(Self::Allocation),
            "collection" => Ok(Self::Collection),
            "pin.enter" => Ok(Self::PinEnter),
            "pin.exit" => Ok(Self::PinExit),
            "yield" => Ok(Self::Yield),
            "trap" => Ok(Self::Trap),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RootMapDescriptor {
    pub safe_point: StringId,
    pub kind: SafePointKind,
    pub procedure: Option<ProcedureId>,
    pub local_slots: RootSlotList,
    pub stack_slots: RootSlotList,
    pub capture_slots: RootSlotList,
    pub defer_slots: RootSlotList,
    pub pin_slots: RootSlotList,
}

impl RootMapDescriptor {
    #[must_use]
    pub fn new(safe_point: StringId, local_slots: RootSlotList, stack_slots: RootSlotList) -> Self {
        Self {
            safe_point,
            kind: SafePointKind::Call,
            procedure: None,
            local_slots,
            stack_slots,
            capture_slots: Box::new([]),
            defer_slots: Box::new([]),
            pin_slots: Box::new([]),
        }
    }

    #[must_use]
    pub const fn with_procedure(mut self, procedure: ProcedureId) -> Self {
        self.procedure = Some(procedure);
        self
    }

    #[must_use]
    pub const fn with_kind(mut self, kind: SafePointKind) -> Self {
        self.kind = kind;
        self
    }

    #[must_use]
    pub fn with_capture_slots(mut self, capture_slots: RootSlotList) -> Self {
        self.capture_slots = capture_slots;
        self
    }

    #[must_use]
    pub fn with_defer_slots(mut self, defer_slots: RootSlotList) -> Self {
        self.defer_slots = defer_slots;
        self
    }

    #[must_use]
    pub fn with_pin_slots(mut self, pin_slots: RootSlotList) -> Self {
        self.pin_slots = pin_slots;
        self
    }
}
