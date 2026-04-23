#[cfg(any(test, debug_assertions))]
pub fn validate(text: &str) {
    if text.is_empty() {
        return;
    }

    assert!(
        !text.contains(": "),
        "diagnostic text contains ': ': {text:?}"
    );
    assert!(
        text != "type mismatch",
        "diagnostic text lacks expected and found types: {text:?}"
    );
    assert!(
        !is_bare_vague_text(text),
        "diagnostic text lacks concrete subject: {text:?}"
    );
    assert!(
        !contains_unrendered_placeholder(text),
        "diagnostic text contains unrendered template placeholder: {text:?}"
    );

    for word in text.split(|c: char| !c.is_ascii_alphabetic()) {
        assert!(
            !matches!(word, "a" | "an" | "the"),
            "diagnostic text contains article {word:?}: {text:?}"
        );
    }
}

#[cfg(any(test, debug_assertions))]
fn is_bare_vague_text(text: &str) -> bool {
    matches!(
        text,
        "unknown field"
            | "unknown export"
            | "unknown effect"
            | "unknown effect operation"
            | "invalid target"
            | "invalid ask target"
            | "arity mismatch"
            | "call arity mismatch"
            | "type mismatch"
    )
}

#[cfg(any(test, debug_assertions))]
fn contains_unrendered_placeholder(text: &str) -> bool {
    let bytes = text.as_bytes();
    let mut open = 0;
    while open < bytes.len() {
        let Some(open_offset) = bytes[open..].iter().position(|byte| *byte == b'{') else {
            return false;
        };
        open += open_offset;
        let Some(close_offset) = bytes[open..].iter().position(|byte| *byte == b'}') else {
            return false;
        };
        let close = open + close_offset;
        let name = &bytes[open + 1..close];
        if !name.is_empty()
            && name
                .iter()
                .all(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
            && name[0].is_ascii_alphabetic()
        {
            return true;
        }
        open = close.saturating_add(1);
    }
    false
}

#[cfg(not(any(test, debug_assertions)))]
pub const fn validate(text: &str) {
    let _ = text;
}
