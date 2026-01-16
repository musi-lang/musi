#[derive(Debug)]
pub struct Interner {
    strings: Vec<String>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Name(pub(crate) u32);

impl Name {
    #[must_use]
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    #[must_use]
    pub const fn as_u32(self) -> u32 {
        self.0
    }
}

impl Interner {
    #[must_use]
    pub const fn new() -> Self {
        Self { strings: vec![] }
    }

    /// # Panics
    pub fn intern(&mut self, s: &str) -> Name {
        let id = self
            .strings
            .iter()
            .position(|existing| existing == s)
            .unwrap_or_else(|| {
                self.strings.push(s.to_owned());
                self.strings.len() - 1
            });

        Name(id.try_into().expect("interner overflow: too many strings"))
    }

    /// # Panics
    #[must_use]
    pub fn resolve(&self, name: Name) -> &str {
        self.strings
            .get(usize::try_from(name.0).expect("invalid Name ID"))
            .map_or("", String::as_str)
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.strings.len()
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
