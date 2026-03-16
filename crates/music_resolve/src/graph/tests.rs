use music_shared::{FileId, Interner};

use crate::error::ResolveError;
use crate::graph::{ModuleGraph, ModuleId};

fn dummy_file_id() -> FileId {
    FileId(0)
}

fn make_graph_linear() -> ModuleGraph {
    let mut interner = Interner::new();
    let mut graph = ModuleGraph::new();

    let c = graph.add_module("c.ms".into(), String::new(), dummy_file_id());
    let b = graph.add_module("b.ms".into(), String::new(), FileId(1));
    let a = graph.add_module("a.ms".into(), String::new(), FileId(2));

    let sym_b = interner.intern("./b");
    let sym_c = interner.intern("./c");

    graph.add_edge(a, b, sym_b);
    graph.add_edge(b, c, sym_c);

    graph
}

#[test]
fn test_toposort_linear_chain() {
    let graph = make_graph_linear();
    let order = graph.toposort().unwrap();
    assert_eq!(order.len(), 3);
    assert_eq!(order[0], ModuleId(0));
    assert_eq!(order[1], ModuleId(1));
    assert_eq!(order[2], ModuleId(2));
}

#[test]
fn test_toposort_diamond() {
    let mut interner = Interner::new();
    let mut graph = ModuleGraph::new();

    let d = graph.add_module("d.ms".into(), String::new(), FileId(0));
    let b = graph.add_module("b.ms".into(), String::new(), FileId(1));
    let c = graph.add_module("c.ms".into(), String::new(), FileId(2));
    let a = graph.add_module("a.ms".into(), String::new(), FileId(3));

    let sym_b = interner.intern("./b");
    let sym_c = interner.intern("./c");
    let sym_d = interner.intern("./d");

    graph.add_edge(a, b, sym_b);
    graph.add_edge(a, c, sym_c);
    graph.add_edge(b, d, sym_d);
    graph.add_edge(c, d, sym_d);

    let order = graph.toposort().unwrap();
    assert_eq!(order.len(), 4);

    let pos = |id: ModuleId| order.iter().position(|&x| x == id).unwrap();
    assert!(pos(d) < pos(b));
    assert!(pos(d) < pos(c));
    assert!(pos(b) < pos(a));
    assert!(pos(c) < pos(a));
}

#[test]
fn test_toposort_cycle_returns_error() {
    let mut interner = Interner::new();
    let mut graph = ModuleGraph::new();

    let a = graph.add_module("a.ms".into(), String::new(), FileId(0));
    let b = graph.add_module("b.ms".into(), String::new(), FileId(1));

    let sym_a = interner.intern("./a");
    let sym_b = interner.intern("./b");

    graph.add_edge(a, b, sym_b);
    graph.add_edge(b, a, sym_a);

    let err = graph.toposort().unwrap_err();
    assert!(matches!(err, ResolveError::CircularImport { .. }));
}

#[test]
fn test_toposort_self_import_returns_error() {
    let mut interner = Interner::new();
    let mut graph = ModuleGraph::new();

    let a = graph.add_module("a.ms".into(), String::new(), FileId(0));
    let sym_a = interner.intern("./a");
    graph.add_edge(a, a, sym_a);

    let err = graph.toposort().unwrap_err();
    assert!(matches!(err, ResolveError::CircularImport { .. }));
}

#[test]
fn test_empty_graph() {
    let graph = ModuleGraph::new();
    let order = graph.toposort().unwrap();
    assert!(order.is_empty());
}

#[test]
fn test_single_module_no_imports() {
    let mut graph = ModuleGraph::new();
    let _a = graph.add_module("a.ms".into(), String::new(), FileId(0));
    let order = graph.toposort().unwrap();
    assert_eq!(order.len(), 1);
}

#[test]
fn test_dedup_same_path() {
    let mut graph = ModuleGraph::new();
    let a1 = graph.add_module("a.ms".into(), String::new(), FileId(0));
    let a2 = graph.add_module("a.ms".into(), "different source".into(), FileId(1));
    assert_eq!(a1, a2);
    assert_eq!(graph.len(), 1);
}
