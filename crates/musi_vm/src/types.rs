use super::VmError;

pub type VmResult<T = ()> = Result<T, VmError>;
