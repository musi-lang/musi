use crate::span::Span;
use std::error;
use std::fmt;

pub type MusiResult<T> = Result<T, MusiError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ErrorCode {
    MS1001,
    MS1002,
    MS1003,
    MS1004,
    MS1005,
    MS1006,
    MS1007,
    MS1008,
    MS1009,
    MS1010,
    MS1011,

    MS2001,
    MS2002,
    MS2003,
    MS2004,
    MS2005,
    MS2006,
    MS2007,
    MS2008,
    MS2009,
    MS2010,
    MS2011,
    MS2012,
    MS2013,

    MS3001,
    MS3002,
    MS3003,
    MS3004,
    MS3005,
    MS3006,
    MS3007,
    MS3008,
    MS3009,
    MS3010,
    MS3011,
    MS3012,
    MS3013,
    MS3014,
    MS3015,
    MS3016,
    MS3017,

    MS4001,
    MS4002,

    MS5001,
    MS5002,
}

impl ErrorCode {
    #[must_use]
    pub const fn as_u16(self) -> u16 {
        match self {
            Self::MS1001 => 1001,
            Self::MS1002 => 1002,
            Self::MS1003 => 1003,
            Self::MS1004 => 1004,
            Self::MS1005 => 1005,
            Self::MS1006 => 1006,
            Self::MS1007 => 1007,
            Self::MS1008 => 1008,
            Self::MS1009 => 1009,
            Self::MS1010 => 1010,
            Self::MS1011 => 1011,
            Self::MS2001 => 2001,
            Self::MS2002 => 2002,
            Self::MS2003 => 2003,
            Self::MS2004 => 2004,
            Self::MS2005 => 2005,
            Self::MS2006 => 2006,
            Self::MS2007 => 2007,
            Self::MS2008 => 2008,
            Self::MS2009 => 2009,
            Self::MS2010 => 2010,
            Self::MS2011 => 2011,
            Self::MS2012 => 2012,
            Self::MS2013 => 2013,
            Self::MS3001 => 3001,
            Self::MS3002 => 3002,
            Self::MS3003 => 3003,
            Self::MS3004 => 3004,
            Self::MS3005 => 3005,
            Self::MS3006 => 3006,
            Self::MS3007 => 3007,
            Self::MS3008 => 3008,
            Self::MS3009 => 3009,
            Self::MS3010 => 3010,
            Self::MS3011 => 3011,
            Self::MS3012 => 3012,
            Self::MS3013 => 3013,
            Self::MS3014 => 3014,
            Self::MS3015 => 3015,
            Self::MS3016 => 3016,
            Self::MS3017 => 3017,
            Self::MS4001 => 4001,
            Self::MS4002 => 4002,
            Self::MS5001 => 5001,
            Self::MS5002 => 5002,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MS{}", self.as_u16())
    }
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct MusiError {
    pub code: Option<ErrorCode>,
    pub message: String,
    pub hint: Option<&'static str>,
    pub level: Level,
    pub span: Span,
}

impl MusiError {
    #[must_use]
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            code: None,
            message: message.into(),
            hint: None,
            level: Level::Error,
            span,
        }
    }

    #[must_use]
    pub const fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    #[must_use]
    pub const fn with_hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    #[must_use]
    pub const fn with_level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }
}

impl fmt::Display for MusiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl error::Error for MusiError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Level {
    Error,
    Warning,
    Note,
}

pub trait IntoMusiError: fmt::Display {
    fn code(&self) -> Option<ErrorCode> {
        None
    }

    fn hint(&self) -> Option<&'static str> {
        None
    }

    fn level(&self) -> Level {
        Level::Error
    }

    fn into_musi_error(self, span: Span) -> MusiError
    where
        Self: Sized,
    {
        let mut err = MusiError::new(self.to_string(), span).with_level(self.level());
        if let Some(code) = self.code() {
            err = err.with_code(code);
        }
        if let Some(hint) = self.hint() {
            err = err.with_hint(hint);
        }
        err
    }
}
