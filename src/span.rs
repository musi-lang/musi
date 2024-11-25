use crate::location::Location;

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}
