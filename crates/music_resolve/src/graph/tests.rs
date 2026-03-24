use std::path::PathBuf;

use super::*;

fn path(name: &str) -> PathBuf {
    PathBuf::from(format!("/project/{name}.ms"))
}

#[test]
fn add_module_returns_same_id_for_same_path() {
    let mut graph = ModuleGraph::new();
    let id1 = graph.add_module(path("a"));
    let id2 = graph.add_module(path("a"));
    assert_eq!(id1, id2);
    assert_eq!(graph.len(), 1);
}

#[test]
fn add_module_returns_different_ids_for_different_paths() {
    let mut graph = ModuleGraph::new();
    let id1 = graph.add_module(path("a"));
    let id2 = graph.add_module(path("b"));
    assert_ne!(id1, id2);
    assert_eq!(graph.len(), 2);
}

#[test]
fn loading_state_tracking() {
    let mut graph = ModuleGraph::new();
    let id = graph.add_module(path("a"));

    assert!(graph.is_loading(id));
    assert!(!graph.is_loaded(id));
    assert!(graph.get_exports(id).is_none());
}

#[test]
fn loaded_state_with_exports() {
    let mut graph = ModuleGraph::new();
    let id = graph.add_module(path("a"));

    let exports = ModuleExports::new();
    graph.mark_loaded(id, exports);

    assert!(!graph.is_loading(id));
    assert!(graph.is_loaded(id));
    assert!(graph.get_exports(id).is_some());
}

#[test]
fn linear_chain_topo_sort() {
    // A -> B -> C
    // Expected order: C, B, A (deps first)
    let mut graph = ModuleGraph::new();
    let a = graph.add_module(path("a"));
    let b = graph.add_module(path("b"));
    let c = graph.add_module(path("c"));

    graph.add_edge(a, b);
    graph.add_edge(b, c);

    let order = graph.topo_sort().unwrap();
    assert_eq!(order.len(), 3);

    let pos_a = order.iter().position(|&id| id == a).unwrap();
    let pos_b = order.iter().position(|&id| id == b).unwrap();
    let pos_c = order.iter().position(|&id| id == c).unwrap();

    // C before B before A
    assert!(pos_c < pos_b);
    assert!(pos_b < pos_a);
}

#[test]
fn diamond_topo_sort() {
    //   A
    //  / \
    // B   C
    //  \ /
    //   D
    let mut graph = ModuleGraph::new();
    let a = graph.add_module(path("a"));
    let b = graph.add_module(path("b"));
    let c = graph.add_module(path("c"));
    let d = graph.add_module(path("d"));

    graph.add_edge(a, b);
    graph.add_edge(a, c);
    graph.add_edge(b, d);
    graph.add_edge(c, d);

    let order = graph.topo_sort().unwrap();
    assert_eq!(order.len(), 4);

    let pos_a = order.iter().position(|&id| id == a).unwrap();
    let pos_b = order.iter().position(|&id| id == b).unwrap();
    let pos_c = order.iter().position(|&id| id == c).unwrap();
    let pos_d = order.iter().position(|&id| id == d).unwrap();

    // D must come before B and C; B and C before A
    assert!(pos_d < pos_b);
    assert!(pos_d < pos_c);
    assert!(pos_b < pos_a);
    assert!(pos_c < pos_a);
}

#[test]
fn cycle_detected_two_modules() {
    // A -> B -> A
    let mut graph = ModuleGraph::new();
    let a = graph.add_module(path("a"));
    let b = graph.add_module(path("b"));

    graph.add_edge(a, b);
    graph.add_edge(b, a);

    let err = graph.topo_sort().unwrap_err();
    assert_eq!(err.modules.len(), 2);
    assert!(err.modules.contains(&a));
    assert!(err.modules.contains(&b));
}

#[test]
fn self_import_cycle() {
    // A -> A
    let mut graph = ModuleGraph::new();
    let a = graph.add_module(path("a"));
    graph.add_edge(a, a);

    let err = graph.topo_sort().unwrap_err();
    assert_eq!(err.modules.len(), 1);
    assert!(err.modules.contains(&a));
}

#[test]
fn cycle_chain_reports_path() {
    // A -> B -> C -> A
    let mut graph = ModuleGraph::new();
    let a = graph.add_module(path("a"));
    let b = graph.add_module(path("b"));
    let c = graph.add_module(path("c"));

    graph.add_edge(a, b);
    graph.add_edge(b, c);
    graph.add_edge(c, a);

    let chain = graph.find_cycle_chain(a);
    assert!(!chain.is_empty());
    // Chain should start and end with the same module
    assert_eq!(chain.first(), chain.last());
}

#[test]
fn isolated_modules_sort_succeeds() {
    let mut graph = ModuleGraph::new();
    let _a = graph.add_module(path("a"));
    let _b = graph.add_module(path("b"));
    let _c = graph.add_module(path("c"));

    let order = graph.topo_sort().unwrap();
    assert_eq!(order.len(), 3);
}

#[test]
fn duplicate_edges_ignored() {
    let mut graph = ModuleGraph::new();
    let a = graph.add_module(path("a"));
    let b = graph.add_module(path("b"));

    graph.add_edge(a, b);
    graph.add_edge(a, b);

    assert_eq!(graph.deps(a).len(), 1);
}

#[test]
fn lookup_by_path() {
    let mut graph = ModuleGraph::new();
    let id = graph.add_module(path("a"));

    assert_eq!(graph.lookup(&path("a")), Some(id));
    assert_eq!(graph.lookup(&path("nonexistent")), None);
}

#[test]
fn path_roundtrip() {
    let mut graph = ModuleGraph::new();
    let p = path("my_module");
    let id = graph.add_module(p.clone());
    assert_eq!(graph.path(id), p);
}

#[test]
fn empty_graph() {
    let graph = ModuleGraph::new();
    assert!(graph.is_empty());
    assert_eq!(graph.len(), 0);
    let order = graph.topo_sort().unwrap();
    assert!(order.is_empty());
}

#[test]
fn complex_dag_topo_sort() {
    // E depends on nothing
    // D -> E
    // B -> D
    // C -> D, C -> E
    // A -> B, A -> C
    let mut graph = ModuleGraph::new();
    let mod_a = graph.add_module(path("a"));
    let mod_b = graph.add_module(path("b"));
    let mod_c = graph.add_module(path("c"));
    let mod_d = graph.add_module(path("d"));
    let mod_e = graph.add_module(path("e"));

    graph.add_edge(mod_a, mod_b);
    graph.add_edge(mod_a, mod_c);
    graph.add_edge(mod_b, mod_d);
    graph.add_edge(mod_c, mod_d);
    graph.add_edge(mod_c, mod_e);
    graph.add_edge(mod_d, mod_e);

    let order = graph.topo_sort().unwrap();
    assert_eq!(order.len(), 5);

    let pos = |id: ModuleId| order.iter().position(|&x| x == id).unwrap();

    // E before D, D before B, D before C, B before A, C before A
    assert!(pos(mod_e) < pos(mod_d));
    assert!(pos(mod_d) < pos(mod_b));
    assert!(pos(mod_d) < pos(mod_c));
    assert!(pos(mod_b) < pos(mod_a));
    assert!(pos(mod_c) < pos(mod_a));
}
