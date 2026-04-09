use crate::AssemblyError;

pub type AssemblyResult<T = ()> = Result<T, AssemblyError>;
