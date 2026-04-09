use crate::ProjectError;

pub type ProjectResult<T = ()> = Result<T, ProjectError>;
