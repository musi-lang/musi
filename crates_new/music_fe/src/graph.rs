use std::collections::{BTreeSet, HashMap, VecDeque};

use music_basic::{SourceId, SourceMap};
use music_lex::Lexer;
use music_parse::ParsedSource;

use crate::errors::{FrontendError, FrontendErrorKind, FrontendResult};

pub struct ModuleNode {
    pub source_id: SourceId,
    pub path: String,
    pub imports: Box<[String]>,
}

pub struct ModuleGraph {
    pub nodes_in_order: Box<[ModuleNode]>,
}

pub fn build_module_graph(sources: &SourceMap, entry: SourceId) -> FrontendResult<ModuleGraph> {
    let entry_source = sources.get(entry).ok_or(FrontendError {
        kind: FrontendErrorKind::EntrySourceMissing,
    })?;
    let entry_path = entry_source.path().to_string_lossy().into_owned();

    let mut seen = BTreeSet::<String>::new();
    let mut queue = VecDeque::<String>::new();
    queue.push_back(entry_path.clone());
    let _ = seen.insert(entry_path.clone());

    let mut nodes = Vec::<ModuleNode>::new();
    let mut node_by_path = HashMap::<String, usize>::new();

    while let Some(path) = queue.pop_front() {
        let source_id = find_source_id_by_path(sources, &path).ok_or(FrontendError {
            kind: FrontendErrorKind::ImportTargetMissing,
        })?;

        let source = sources.get(source_id).ok_or(FrontendError {
            kind: FrontendErrorKind::ImportTargetMissing,
        })?;

        let lexed = Lexer::new(source.text()).lex();
        let parsed = music_parse::parse(source_id, &lexed);

        // Import discovery is syntax-only.
        let imports = collect_import_paths(sources, &parsed);

        let idx = nodes.len();
        nodes.push(ModuleNode {
            source_id,
            path: path.clone(),
            imports: imports.clone().into_boxed_slice(),
        });
        let _prev = node_by_path.insert(path.clone(), idx);

        for dep in imports {
            if seen.insert(dep.clone()) {
                queue.push_back(dep);
            }
        }
    }

    // Topo sort by paths using the discovered import edges.
    let order = topo_sort(&nodes, &node_by_path).ok_or(FrontendError {
        kind: FrontendErrorKind::ImportCycle,
    })?;

    let mut nodes = nodes.into_iter().map(Some).collect::<Vec<_>>();
    let nodes_in_order = order
        .into_iter()
        .map(|i| nodes[i].take().expect("topo sort unique indices"))
        .collect();
    Ok(ModuleGraph { nodes_in_order })
}

fn find_source_id_by_path(sources: &SourceMap, path: &str) -> Option<SourceId> {
    sources.iter().find_map(|source| {
        let p = source.path().to_string_lossy();
        (p.as_ref() == path).then_some(source.id())
    })
}

fn collect_import_paths(sources: &SourceMap, parsed: &ParsedSource) -> Vec<String> {
    let mut out = Vec::new();
    let tree = parsed.tree();
    let root = tree.root();
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        if node.kind() == music_ast::SyntaxNodeKind::ImportExpr {
            let path_tok = node
                .child_tokens()
                .find(|t| matches!(t.kind(), music_lex::TokenKind::StringLit));
            if let Some(path_tok) = path_tok {
                if let Some(source) = sources.get(tree.source_id()) {
                    let start = usize::try_from(path_tok.span().start).unwrap_or(0);
                    let end = usize::try_from(path_tok.span().end).unwrap_or(start);
                    let raw = source.text().get(start..end).unwrap_or("");
                    out.push(music_basic::string_lit::decode(raw));
                }
            }
        }
        for child in node.child_nodes() {
            stack.push(child);
        }
    }
    out
}

fn topo_sort(nodes: &[ModuleNode], node_by_path: &HashMap<String, usize>) -> Option<Vec<usize>> {
    let mut indeg = vec![0usize; nodes.len()];
    let mut edges: Vec<Vec<usize>> = vec![Vec::new(); nodes.len()];

    for (i, node) in nodes.iter().enumerate() {
        for dep in node.imports.iter() {
            let j = *node_by_path.get(dep)?;
            edges[j].push(i);
            indeg[i] += 1;
        }
    }

    let mut q = VecDeque::new();
    for (i, d) in indeg.iter().copied().enumerate() {
        if d == 0 {
            q.push_back(i);
        }
    }

    let mut out = Vec::with_capacity(nodes.len());
    while let Some(i) = q.pop_front() {
        out.push(i);
        for &to in edges[i].iter() {
            indeg[to] -= 1;
            if indeg[to] == 0 {
                q.push_back(to);
            }
        }
    }

    (out.len() == nodes.len()).then_some(out)
}
