//! Request handlers for the LSP server.

use std::fs::read_to_string;

use musi_tooling::{ToolLocation, ToolPosition, ToolWorkspaceEdit};

use super::*;

impl MusiLanguageServer {
    pub(super) fn completions(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let text_document = params.text_document_position.text_document;
        let position = params.text_document_position.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tool_position = to_tool_position_in_text(text, position)?;
        let items = completions_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )
        .into_iter()
        .map(|completion| to_lsp_completion(text, completion))
        .collect();
        Some(CompletionResponse::List(CompletionList {
            is_incomplete: false,
            items,
        }))
    }

    pub(super) fn resolve_completion(completion: CompletionItem) -> CompletionItem {
        resolve_lsp_completion(completion)
    }

    pub(super) fn hover_at(&self, params: HoverParams) -> Option<Hover> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        if let Some(hover) = self.import_hover_at(&path, Some(text), position) {
            return Some(hover);
        }
        let tool_position = to_tool_position_in_text(text, position)?;
        let hover = hover_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )?;
        let contents = truncate_hover_contents(&hover.contents, self.config.hover_maximum_length);
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: Some(to_lsp_range_in_text(text, &hover.range)),
        })
    }

    pub(super) fn import_hover_at(
        &self,
        path: &Path,
        overlay: Option<&str>,
        position: Position,
    ) -> Option<Hover> {
        let link = document_links_for_project_file_with_overlay(path, overlay)
            .into_iter()
            .find(|link| position_in_lsp_range(position, to_tool_range(&link.range)))?;
        let mut contents = format!(
            "```musi\n(module) {}\n```\n\nResolves to `{}`.",
            link.specifier, link.resolved
        );
        if let Some(docs) = module_docs_for_project_file_with_overlay(&link.target, None) {
            contents.push_str("\n\n");
            contents.push_str(&docs);
        }
        let contents = truncate_hover_contents(&contents, self.config.hover_maximum_length);
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: Some(to_lsp_range_in_text(overlay?, &link.range)),
        })
    }

    pub(super) fn signature_help_at(&self, params: SignatureHelpParams) -> Option<SignatureHelp> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tool_position = to_tool_position_in_text(text, position)?;
        signature_help_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )
        .map(to_lsp_signature_help)
    }

    pub(super) fn definition_at(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<GotoDefinitionResponse> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        if let Some(location) = import_definition_at(&path, text, position) {
            return Some(GotoDefinitionResponse::Scalar(location));
        }
        let tool_position = to_tool_position_in_text(text, position)?;
        let location = definition_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )
        .and_then(|location| self.lsp_location_for_tool_location(&location))?;
        Some(GotoDefinitionResponse::Scalar(location))
    }

    pub(super) fn type_definition_at(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<GotoDefinitionResponse> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tool_position = to_tool_position_in_text(text, position)?;
        let location = type_definition_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )
        .and_then(|location| self.lsp_location_for_tool_location(&location))?;
        Some(GotoDefinitionResponse::Scalar(location))
    }

    pub(super) fn implementation_at(
        &self,
        params: GotoDefinitionParams,
    ) -> Option<GotoDefinitionResponse> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tool_position = to_tool_position_in_text(text, position)?;
        let locations = implementation_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )
        .into_iter()
        .filter_map(|location| self.lsp_location_for_tool_location(&location))
        .collect::<Vec<_>>();
        (!locations.is_empty()).then_some(GotoDefinitionResponse::Array(locations))
    }

    pub(super) fn references_at(&mut self, params: ReferenceParams) -> Option<Vec<Location>> {
        let text_document = params.text_document_position.text_document;
        let position = params.text_document_position.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let overlay = self
            .open_documents
            .get(&text_document.uri)
            .map(String::as_str);
        let text = if let Some(overlay) = overlay {
            overlay
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        if let Some(locations) = self.import_references_at(&path, text, position) {
            return Some(locations);
        }
        let tool_position = to_tool_position_in_text(text, position)?;
        let tool_locations = self
            .navigation_workspace
            .references_for_project_file_with_overlay(
                &path,
                overlay,
                tool_position.line,
                tool_position.col,
                params.context.include_declaration,
            );
        let locations = self.lsp_locations_for_tool_locations(tool_locations);
        Some(locations)
    }

    pub(super) fn import_references_at(
        &self,
        path: &Path,
        text: &str,
        position: Position,
    ) -> Option<Vec<Location>> {
        let target = document_links_for_project_file_with_overlay(path, Some(text))
            .into_iter()
            .find(|link| position_in_lsp_range(position, to_lsp_range_in_text(text, &link.range)))?
            .target;
        let mut locations = Vec::new();
        for candidate_path in self.workspace_source_paths() {
            let open = self.open_document_for_path(&candidate_path);
            let candidate_overlay = open.map(|(_, text)| text);
            let Some(uri) = open
                .map(|(uri, _)| uri.clone())
                .or_else(|| Url::from_file_path(&candidate_path).ok())
            else {
                continue;
            };
            let file_text;
            let candidate_text = if let Some(text) = candidate_overlay {
                text
            } else {
                file_text = read_to_string(&candidate_path).ok()?;
                file_text.as_str()
            };
            locations.extend(
                document_links_for_project_file_with_overlay(&candidate_path, Some(candidate_text))
                    .into_iter()
                    .filter(|link| paths_match(&link.target, &target))
                    .map(|link| Location {
                        uri: uri.clone(),
                        range: to_lsp_range_in_text(candidate_text, &link.range),
                    }),
            );
        }
        locations.sort_by_key(|location| {
            (
                location.uri.to_string(),
                location.range.start.line,
                location.range.start.character,
            )
        });
        locations.dedup_by_key(|location| {
            (
                location.uri.to_string(),
                location.range.start.line,
                location.range.start.character,
            )
        });
        Some(locations)
    }

    pub(super) fn monikers_at(&self, params: MonikerParams) -> Option<Vec<Moniker>> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        if let Some(moniker) = import_moniker_at(&path, text, position) {
            return Some(vec![moniker]);
        }
        let moniker = moniker_for_project_file_with_overlay(
            &path,
            Some(text),
            usize::try_from(position.line).ok()?.saturating_add(1),
            usize::try_from(position.character).ok()?.saturating_add(1),
        )?;
        let uri = Url::from_file_path(moniker.location.path).ok()?;
        Some(vec![Moniker {
            scheme: "musi".to_owned(),
            identifier: format!(
                "{}#{}:{}",
                uri.as_str(),
                moniker.location.range.start_line,
                moniker.location.range.start_col
            ),
            unique: UniquenessLevel::Project,
            kind: Some(match moniker.kind {
                ToolMonikerKind::Import => MonikerKind::Import,
                ToolMonikerKind::Local => MonikerKind::Local,
            }),
        }])
    }

    pub(super) fn document_highlights(
        &self,
        params: DocumentHighlightParams,
    ) -> Option<Vec<DocumentHighlight>> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        if let Some(highlights) = import_document_highlights(&path, text, position) {
            return Some(highlights);
        }
        let tool_position = to_tool_position_in_text(text, position)?;
        let highlights = document_highlights_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )
        .into_iter()
        .filter(|highlight| tool_location_matches_path(&path, &highlight.location))
        .map(|highlight| {
            let range = to_lsp_range_in_text(text, &highlight.location.range);
            DocumentHighlight {
                range,
                kind: Some(to_lsp_document_highlight_kind(highlight.kind)),
            }
        })
        .collect();
        Some(highlights)
    }

    pub(super) fn linked_editing_ranges(
        &self,
        params: LinkedEditingRangeParams,
    ) -> Option<LinkedEditingRanges> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        if let Some(ranges) = import_linked_editing_ranges(&path, text, position) {
            return Some(ranges);
        }
        let tool_position = to_tool_position_in_text(text, position)?;
        let ranges = references_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
            true,
        )
        .into_iter()
        .filter(|location| tool_location_matches_path(&path, location))
        .map(|location| to_lsp_range_in_text(text, &location.range))
        .collect::<Vec<_>>();
        (ranges.len() > 1).then_some(LinkedEditingRanges {
            ranges,
            word_pattern: Some("[A-Za-z_][A-Za-z0-9_]*".to_owned()),
        })
    }

    pub(super) fn prepare_call_hierarchy_at(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Option<Vec<CallHierarchyItem>> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self
            .open_documents
            .get(&text_document.uri)
            .map(String::as_str);
        let symbols = document_symbols_for_project_file_with_overlay(&path, overlay);
        let symbol = symbol_at_position(&symbols, position)?;
        Some(vec![to_lsp_call_hierarchy_item(&text_document.uri, symbol)])
    }

    pub(super) fn call_hierarchy_incoming_calls(
        &self,
        params: &CallHierarchyIncomingCallsParams,
    ) -> Option<Vec<CallHierarchyIncomingCall>> {
        let (uri, line, character) = call_hierarchy_item_data_parts(&params.item)?;
        let path = uri.to_file_path().ok()?;
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let mut calls = Vec::<CallHierarchyIncomingCall>::new();
        for location in references_for_project_file_with_overlay(
            &path,
            overlay,
            line.saturating_add(1),
            character.saturating_add(1),
            false,
        ) {
            let Some(reference_uri) = Url::from_file_path(&location.path).ok() else {
                continue;
            };
            let reference_overlay = self.open_documents.get(&reference_uri).map(String::as_str);
            let symbols =
                document_symbols_for_project_file_with_overlay(&location.path, reference_overlay);
            let Some(symbol) = caller_symbol_for_reference(&symbols, &location.range) else {
                continue;
            };
            let from = to_lsp_call_hierarchy_item(&reference_uri, symbol);
            let from_range = to_tool_range(&location.range);
            if let Some(call) = calls
                .iter_mut()
                .find(|call| call_hierarchy_items_match(&call.from, &from))
            {
                call.from_ranges.push(from_range);
            } else {
                calls.push(CallHierarchyIncomingCall {
                    from,
                    from_ranges: vec![from_range],
                });
            }
        }
        Some(calls)
    }

    pub(super) fn call_hierarchy_outgoing_calls(
        &self,
        params: &CallHierarchyOutgoingCallsParams,
    ) -> Option<Vec<CallHierarchyOutgoingCall>> {
        let (uri, line, character) = call_hierarchy_item_data_parts(&params.item)?;
        let path = uri.to_file_path().ok()?;
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let calls = outgoing_calls_for_project_file_with_overlay(
            &path,
            overlay,
            line.saturating_add(1),
            character.saturating_add(1),
        )
        .into_iter()
        .filter_map(|call| {
            let uri = Url::from_file_path(call.to.location.path).ok()?;
            Some(CallHierarchyOutgoingCall {
                to: CallHierarchyItem {
                    name: call.to.name,
                    kind: to_lsp_symbol_kind(call.to.kind),
                    tags: None,
                    detail: None,
                    uri: uri.clone(),
                    range: to_tool_range(&call.to.location.range),
                    selection_range: to_tool_range(&call.to.location.range),
                    data: Some(json!({
                        "uri": uri.as_str(),
                        "line": call.to.location.range.start_line.saturating_sub(1),
                        "character": call.to.location.range.start_col.saturating_sub(1),
                    })),
                },
                from_ranges: call
                    .from_ranges
                    .into_iter()
                    .map(|range| to_tool_range(&range))
                    .collect(),
            })
        })
        .collect();
        Some(calls)
    }

    pub(super) fn document_links(&self, params: DocumentLinkParams) -> Option<Vec<DocumentLink>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let links = document_links_for_project_file_with_overlay(&path, Some(text))
            .into_iter()
            .filter_map(|link| to_lsp_document_link(text, &link))
            .collect();
        Some(links)
    }

    pub(super) fn resolve_document_link(link: DocumentLink) -> DocumentLink {
        resolve_lsp_document_link(link)
    }

    pub(super) fn code_lenses(&mut self, params: CodeLensParams) -> Option<Vec<CodeLens>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let mut lenses = Vec::new();
        for lens in self
            .navigation_workspace
            .reference_lenses_for_project_file_with_overlay(&path, overlay)
        {
            push_reference_lens(&path, &lens, &mut lenses);
        }
        Some(lenses)
    }

    pub(super) fn resolve_code_lens(&mut self, mut lens: CodeLens) -> CodeLens {
        if lens.command.is_some() {
            return lens;
        }
        let Some(data) = lens.data.as_ref() else {
            return lens;
        };
        let Some((uri, line, character)) = reference_lens_data_parts(data) else {
            return lens;
        };
        let Ok(path) = uri.to_file_path() else {
            return lens;
        };
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let tool_locations = self
            .navigation_workspace
            .references_for_project_file_with_overlay(
                &path,
                overlay,
                line.saturating_add(1),
                character.saturating_add(1),
                false,
            );
        let references = self.lsp_locations_for_tool_locations(tool_locations);
        let Ok(line) = u32::try_from(line) else {
            return lens;
        };
        let Ok(character) = u32::try_from(character) else {
            return lens;
        };
        let Ok(locations) = serde_json::to_value(&references) else {
            return lens;
        };
        if references.is_empty() {
            return lens;
        }
        lens.command = Some(Command::new(
            reference_lens_title(references.len()),
            SHOW_REFERENCES_COMMAND.to_owned(),
            Some(vec![
                json!(uri.as_str()),
                json!(Position::new(line, character)),
                locations,
            ]),
        ));
        lens
    }

    pub(super) fn execute_command_request(
        &mut self,
        params: &ExecuteCommandParams,
    ) -> Option<Value> {
        if params.command != REFERENCES_COMMAND {
            return None;
        }
        let argument = params.arguments.first()?;
        let uri = argument.get("uri")?.as_str()?;
        let line = usize::try_from(argument.get("line")?.as_u64()?).ok()?;
        let character = usize::try_from(argument.get("character")?.as_u64()?).ok()?;
        let uri = Url::parse(uri).ok()?;
        let locations = self.references_at(ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(u32::try_from(line).ok()?, u32::try_from(character).ok()?),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: ReferenceContext {
                include_declaration: false,
            },
        })?;
        serde_json::to_value(locations).ok()
    }

    pub(super) fn folding_ranges(&self, params: FoldingRangeParams) -> Option<Vec<FoldingRange>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let ranges = folding_ranges_for_project_file_with_overlay(&path, overlay)
            .into_iter()
            .map(|range| to_lsp_folding_range(&range))
            .collect();
        Some(ranges)
    }

    pub(super) fn selection_ranges(
        &self,
        params: SelectionRangeParams,
    ) -> Option<Vec<SelectionRange>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let positions = params
            .positions
            .into_iter()
            .map(|position| {
                Some(ToolPosition::new(
                    usize::try_from(position.line).ok()?.saturating_add(1),
                    usize::try_from(position.character).ok()?.saturating_add(1),
                ))
            })
            .collect::<Option<Vec<_>>>()?;
        let ranges = selection_ranges_for_project_file_with_overlay(&path, overlay, &positions)
            .into_iter()
            .filter_map(|range| range.map(to_lsp_selection_range))
            .collect();
        Some(ranges)
    }

    pub(super) fn prepare_rename_at(
        &self,
        params: TextDocumentPositionParams,
    ) -> Option<PrepareRenameResponse> {
        let text_document = params.text_document;
        let position = params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tool_position = to_tool_position_in_text(text, position)?;
        let (range, placeholder) = prepare_rename_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
        )?;
        Some(PrepareRenameResponse::RangeWithPlaceholder {
            range: to_lsp_range_in_text(text, &range),
            placeholder,
        })
    }

    pub(super) fn rename_at(&self, params: RenameParams) -> Option<WorkspaceEdit> {
        let text_document = params.text_document_position.text_document;
        let position = params.text_document_position.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(&text_document.uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tool_position = to_tool_position_in_text(text, position)?;
        rename_for_project_file_with_overlay(
            &path,
            Some(text),
            tool_position.line,
            tool_position.col,
            &params.new_name,
        )
        .and_then(|edit| self.lsp_workspace_edit_for_tool_edit(edit))
    }

    pub(super) fn will_rename_files_edit(
        &self,
        params: &RenameFilesParams,
    ) -> Option<WorkspaceEdit> {
        let renames = params
            .files
            .iter()
            .filter_map(|file| {
                let old_uri = Url::parse(&file.old_uri).ok()?;
                let new_uri = Url::parse(&file.new_uri).ok()?;
                Some((old_uri.to_file_path().ok()?, new_uri.to_file_path().ok()?))
            })
            .collect::<Vec<_>>();
        if renames.is_empty() {
            return None;
        }
        let mut changes = HashMap::<Url, Vec<TextEdit>>::new();
        for document_path in self.workspace_source_paths() {
            if document_path
                .file_name()
                .is_some_and(|name| name == "musi.json")
            {
                continue;
            }
            let open_document = self.open_document_for_path(&document_path);
            let uri = open_document.map_or_else(
                || Url::from_file_path(&document_path).ok(),
                |(uri, _)| Some(uri.clone()),
            )?;
            let overlay = open_document.map(|(_, text)| text);
            for link in document_links_for_project_file_with_overlay(&document_path, overlay) {
                let Some(new_target) = renamed_target_path(&renames, &link.target) else {
                    continue;
                };
                let Some(specifier) = import_specifier_for_target(&document_path, &new_target)
                else {
                    continue;
                };
                changes.entry(uri.clone()).or_default().push(TextEdit::new(
                    to_tool_range(&link.range),
                    format!("\"{specifier}\""),
                ));
            }
        }
        (!changes.is_empty()).then_some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }

    pub(super) fn workspace_source_paths(&self) -> Vec<PathBuf> {
        let mut paths = self.workspace_diagnostic_paths();
        for root in self.workspace_query_roots() {
            collect_workspace_source_paths(&root, &mut paths);
        }
        sort_dedup_paths(paths)
    }

    fn lsp_location_for_tool_location(&self, location: &ToolLocation) -> Option<Location> {
        let uri = Url::from_file_path(&location.path).ok()?;
        let file_text;
        let text = if let Some((_, text)) = self.open_document_for_path(&location.path) {
            text
        } else {
            file_text = read_to_string(&location.path).ok()?;
            file_text.as_str()
        };
        Some(Location {
            uri,
            range: to_lsp_range_in_text(text, &location.range),
        })
    }

    fn lsp_locations_for_tool_locations(&self, locations: Vec<ToolLocation>) -> Vec<Location> {
        let mut documents = HashMap::<PathBuf, (Url, String)>::new();
        locations
            .into_iter()
            .filter_map(|location| {
                let (uri, text) = if let Some((uri, text)) =
                    documents.get(&location.path).map(|(uri, text)| (uri, text))
                {
                    (uri.clone(), text.as_str())
                } else {
                    let open_document = self.open_document_for_path(&location.path);
                    let uri = open_document.map_or_else(
                        || Url::from_file_path(&location.path).ok(),
                        |(uri, _)| Some(uri.clone()),
                    )?;
                    let text = open_document
                        .map(|(_, text)| text.to_owned())
                        .or_else(|| read_to_string(&location.path).ok())?;
                    let _ = documents.insert(location.path.clone(), (uri.clone(), text));
                    let (_, text) = documents.get(&location.path)?;
                    (uri, text.as_str())
                };
                Some(Location {
                    uri,
                    range: to_lsp_range_in_text(text, &location.range),
                })
            })
            .collect()
    }

    fn lsp_workspace_edit_for_tool_edit(&self, edit: ToolWorkspaceEdit) -> Option<WorkspaceEdit> {
        let mut changes = HashMap::new();
        for (path, edits) in edit.changes {
            let uri = Url::from_file_path(&path).ok()?;
            let file_text;
            let text = if let Some((_, text)) = self.open_document_for_path(&path) {
                text
            } else {
                file_text = read_to_string(&path).ok()?;
                file_text.as_str()
            };
            let edits = edits
                .into_iter()
                .map(|edit| TextEdit::new(to_lsp_range_in_text(text, &edit.range), edit.new_text))
                .collect::<Vec<_>>();
            let _ = changes.insert(uri, edits);
        }
        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }
}
