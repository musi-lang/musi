pub(crate) fn lowering_error(description: impl Into<Box<str>>) -> Box<str> {
    description.into()
}
