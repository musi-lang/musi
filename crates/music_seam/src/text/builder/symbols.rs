use super::*;

impl TextBuilder {
    pub(crate) fn ensure_type_symbol(&mut self, name: &str, term: &str) -> TypeId {
        if let Some(id) = self.types.get(name).copied() {
            return id;
        }
        let name_id = self.intern_string(name);
        let term_id = self.intern_string(term);
        let ty = self
            .artifact
            .types
            .alloc(TypeDescriptor::new(name_id, term_id));
        let _ = self.types.insert(name.into(), ty);
        ty
    }

    pub(crate) fn ensure_global_symbol(&mut self, name: &str) -> GlobalId {
        if let Some(id) = self.globals.get(name).copied() {
            return id;
        }
        let name_id = self.intern_string(name);
        let id = self.artifact.globals.alloc(GlobalDescriptor::new(name_id));
        let _ = self.globals.insert(name.into(), id);
        id
    }

    pub(crate) fn ensure_procedure_symbol(&mut self, name: &str) -> ProcedureId {
        if let Some(id) = self.procedures.get(name).copied() {
            return id;
        }
        let name_id = self.intern_string(name);
        let id =
            self.artifact
                .procedures
                .alloc(ProcedureDescriptor::new(name_id, 0, 0, Box::new([])));
        let _ = self.procedures.insert(name.into(), id);
        id
    }

    pub(crate) fn intern_string(&mut self, text: &str) -> StringId {
        if let Some(id) = self.strings.get(text).copied() {
            return id;
        }
        let id = self.artifact.intern_string(text);
        let _ = self.strings.insert(text.to_owned(), id);
        id
    }
}

pub(super) fn parse_meta_value(token: &str) -> AssemblyResult<String> {
    if token.starts_with('$') {
        parse_symbol(token)
    } else {
        parse_quoted(token)
    }
}

pub(super) fn ensure_label(
    artifact: &mut Artifact,
    labels: &mut Vec<StringId>,
    label_ids: &mut LabelIdMap,
    name: String,
) -> AssemblyResult<u16> {
    if let Some(id) = label_ids.get(&name).copied() {
        return Ok(id);
    }
    let name_id = artifact.intern_string(&name);
    let id = u16::try_from(labels.len())
        .map_err(|_| text_invalid_operand("label count", labels.len()))?;
    labels.push(name_id);
    let _ = label_ids.insert(name, id);
    Ok(id)
}

pub(super) fn parse_symbol(token: &str) -> AssemblyResult<String> {
    let body = token
        .strip_prefix('$')
        .ok_or_else(|| text_invalid_operand("symbolic name", token))?;
    if body.starts_with('"') {
        parse_quoted(body)
    } else {
        Ok(body.to_owned())
    }
}

pub(super) fn parse_local(token: Option<&String>) -> AssemblyResult<u16> {
    let token = must_get(token, "local")?;
    token
        .strip_prefix('%')
        .ok_or_else(|| text_invalid_operand("local", token))?
        .parse()
        .map_err(|_| text_invalid_operand("local", token))
}

pub(super) fn parse_quoted(token: &str) -> AssemblyResult<String> {
    let Some(body) = token
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
    else {
        return Err(text_invalid_operand("quoted string", token));
    };
    Ok(body.replace("\\\"", "\"").replace("\\\\", "\\"))
}

pub(super) fn must_get<'a>(token: Option<&'a String>, name: &str) -> AssemblyResult<&'a str> {
    token
        .map(String::as_str)
        .ok_or_else(|| text_missing_operand(name))
}

pub(super) fn tokenize(line: &str) -> AssemblyResult<Vec<String>> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut chars = line.chars().peekable();
    let mut in_string = false;

    while let Some(ch) = chars.next() {
        if in_string {
            current.push(ch);
            if ch == '"' && !current.ends_with("\\\"") {
                in_string = false;
                tokens.push(current.clone());
                current.clear();
            }
            continue;
        }
        match ch {
            '"' => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
                current.push(ch);
                in_string = true;
            }
            '$' if matches!(chars.peek().copied(), Some('"')) => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
                current.push('$');
                current.push('"');
                let _ = chars.next();
                in_string = true;
            }
            ' ' | '\t' => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
            }
            _ => current.push(ch),
        }
    }

    if in_string {
        return Err(text_unterminated_string(line));
    }
    if !current.is_empty() {
        tokens.push(current);
    }
    Ok(tokens)
}
