#[cfg(any(test, debug_assertions))]
pub fn validate(text: &str) {
    if text.is_empty() {
        return;
    }

    if text.contains(": ") {
        panic!("diagnostic text contains ': ': {text:?}");
    }

    for word in text.split(|c: char| !c.is_ascii_alphabetic()) {
        match word {
            "a" | "an" | "the" => panic!("diagnostic text contains article {word:?}: {text:?}"),
            _ => {}
        }
    }
}

#[cfg(not(any(test, debug_assertions)))]
pub fn validate(_text: &str) {}
