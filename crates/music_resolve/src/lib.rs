//! Module resolution for the Musi compiler.
//!
//! Parses import specifiers (URI-based schemes like `musi:` (builtins only),
//! `@std/` (standard library), `git:`, `msr:`, relative `./`, and bare names),
//! resolves them to filesystem paths, and builds a module dependency graph with
//! cycle detection and topological sorting.

pub mod builtin;
pub mod error;
pub mod git;
pub mod graph;
pub mod resolver;
pub mod specifier;

pub use error::ResolveError;
pub use graph::{ModuleGraph, ModuleId, ModuleNode, build_module_graph};
pub use resolver::{ResolverConfig, discover_std_root, resolve_import, resolve_relative};
pub use specifier::{GitSource, ImportScheme, ImportSpecifier, parse_git_source, parse_specifier};
