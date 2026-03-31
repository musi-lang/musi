#[cfg(any(test, debug_assertions))]
pub fn validate(text: &str) {
    if text.is_empty() {
        return;
    }

    assert!(
        !text.contains(": "),
        "diagnostic text contains ': ': {text:?}"
    );

    for word in text.split(|c: char| !c.is_ascii_alphabetic()) {
        assert!(
            !matches!(word, "a" | "an" | "the"),
            "diagnostic text contains article {word:?}: {text:?}"
        );
    }
}

#[cfg(not(any(test, debug_assertions)))]
pub fn validate(_text: &str) {}

