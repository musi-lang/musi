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
        !has_suffix_category(text),
        "diagnostic text puts diagnostic category after subject: {text:?}"
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
fn has_suffix_category(text: &str) -> bool {
    category_suffix(text, "unknown")
        || category_suffix(text, "invalid")
        || category_suffix(text, "unsupported")
        || category_suffix(text, "missing")
        || category_suffix(text, "duplicate")
}

#[cfg(any(test, debug_assertions))]
fn category_suffix(text: &str, category: &str) -> bool {
    let Some(index) = text.find(category) else {
        return false;
    };
    if before_backtick_count(text, index) % 2 == 1 {
        return false;
    }
    let after = index + category.len();
    let before_text = text.get(..index).unwrap_or_default();
    let after_text = text.get(after..).unwrap_or_default();
    if before_text
        .chars()
        .next_back()
        .is_some_and(|ch| ch.is_ascii_alphabetic())
        || after_text
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic())
    {
        return false;
    }
    let before = before_text.trim_start();
    if before.is_empty() {
        return false;
    }
    !before_text.contains("not ")
}

#[cfg(any(test, debug_assertions))]
fn before_backtick_count(text: &str, index: usize) -> usize {
    text.get(..index)
        .unwrap_or_default()
        .bytes()
        .filter(|byte| *byte == b'`')
        .count()
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
