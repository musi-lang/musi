pub(super) fn parse_int(text: &str) -> Option<i64> {
    if !text.contains('_') {
        return text.parse::<i64>().ok();
    }
    let filtered: String = text.chars().filter(|c| *c != '_').collect();
    filtered.parse::<i64>().ok()
}

pub(super) fn parse_float(text: &str) -> Option<f64> {
    if !text.contains('_') {
        return text.parse::<f64>().ok();
    }
    let filtered: String = text.chars().filter(|c| *c != '_').collect();
    filtered.parse::<f64>().ok()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
