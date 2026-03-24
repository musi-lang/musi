use super::*;

#[test]
fn empty_object_parses_to_default() {
    let config = load_config_from_str("{}").unwrap();
    assert!(config.name.is_none());
    assert!(config.version.is_none());
    assert!(config.description.is_none());
    assert!(config.main.is_none());
    assert!(config.exports.is_none());
    assert!(config.compiler_options.is_none());
    assert!(config.tasks.is_none());
    assert!(config.workspace.is_none());
}

#[test]
fn partial_config_name_and_version() {
    let json = r#"{ "name": "@app/hello", "version": "1.0.0" }"#;
    let config = load_config_from_str(json).unwrap();
    assert_eq!(config.name.as_deref(), Some("@app/hello"));
    assert_eq!(config.version.as_deref(), Some("1.0.0"));
    assert!(config.description.is_none());
}

#[test]
fn compiler_options_parsing() {
    let json = r#"{
        "compilerOptions": {
            "strict": true,
            "noImplicitAny": false,
            "noUnusedLocals": true,
            "baseUrl": "./src",
            "paths": { "@/*": ["./src/*"] }
        }
    }"#;
    let config = load_config_from_str(json).unwrap();
    let opts = config.compiler_options.unwrap();
    assert_eq!(opts.strict, Some(true));
    assert_eq!(opts.no_implicit_any, Some(false));
    assert_eq!(opts.no_unused_locals, Some(true));
    assert_eq!(opts.base_url.as_deref(), Some("./src"));
    let paths = opts.paths.unwrap();
    assert_eq!(paths["@/*"], vec!["./src/*"]);
}

#[test]
fn task_string_form() {
    let json = r#"{ "tasks": { "build": "musi compile" } }"#;
    let config = load_config_from_str(json).unwrap();
    let tasks = config.tasks.unwrap();
    assert!(matches!(&tasks["build"], Task::Command(cmd) if cmd == "musi compile"));
}

#[test]
fn task_object_form() {
    let json = r#"{
        "tasks": {
            "test": {
                "command": "musi test",
                "description": "Run tests",
                "dependencies": ["build"]
            }
        }
    }"#;
    let config = load_config_from_str(json).unwrap();
    let tasks = config.tasks.unwrap();
    match &tasks["test"] {
        Task::Object(obj) => {
            assert_eq!(obj.command.as_deref(), Some("musi test"));
            assert_eq!(obj.description.as_deref(), Some("Run tests"));
            assert_eq!(
                obj.dependencies.as_deref(),
                Some(["build".to_owned()].as_slice())
            );
        }
        Task::Command(_) => panic!("expected Task::Object"),
    }
}

#[test]
fn invalid_json_returns_error() {
    let result = load_config_from_str("not json");
    assert!(matches!(result, Err(ConfigError::Parse(_))));
}

#[test]
fn exports_as_string() {
    let json = r#"{ "exports": "./index.ms" }"#;
    let config = load_config_from_str(json).unwrap();
    assert!(matches!(config.exports, Some(Exports::Path(ref p)) if p == "./index.ms"));
}

#[test]
fn exports_as_map() {
    let json = r#"{ "exports": { ".": "./mod.ms", "./utils": "./utils.ms" } }"#;
    let config = load_config_from_str(json).unwrap();
    match config.exports {
        Some(Exports::Map(ref map)) => {
            assert_eq!(map["."], "./mod.ms");
            assert_eq!(map["./utils"], "./utils.ms");
        }
        _ => panic!("expected Exports::Map"),
    }
}

#[test]
fn workspace_as_array() {
    let json = r#"{ "workspace": ["./bytes", "./math"] }"#;
    let config = load_config_from_str(json).unwrap();
    match config.workspace {
        Some(Workspace::Members(ref members)) => {
            assert_eq!(members.len(), 2);
            assert_eq!(members[0], "./bytes");
            assert_eq!(members[1], "./math");
        }
        _ => panic!("expected Workspace::Members"),
    }
}

#[test]
fn workspace_as_object() {
    let json = r#"{ "workspace": { "members": ["./a", "./b"] } }"#;
    let config = load_config_from_str(json).unwrap();
    match config.workspace {
        Some(Workspace::Object(ref obj)) => {
            let members = obj.members.as_ref().unwrap();
            assert_eq!(members.len(), 2);
        }
        _ => panic!("expected Workspace::Object"),
    }
}

#[test]
fn full_config_all_fields() {
    let json = r#"{
        "name": "@my/pkg",
        "version": "2.0.0",
        "description": "A test package",
        "main": "src/app.ms",
        "exports": { ".": "./mod.ms" },
        "license": "MIT",
        "author": { "name": "Alice", "email": "alice@example.com" },
        "contributors": ["Bob"],
        "private": true,
        "repository": { "type": "git", "url": "https://github.com/test/repo" },
        "homepage": "https://example.com",
        "bugs": { "url": "https://github.com/test/repo/issues" },
        "keywords": ["test", "example"],
        "imports": { "@/": "./src/" },
        "scopes": { "./src/": { "@lib/": "./lib/" } },
        "dependencies": { "@std/bytes": "^0.1.0" },
        "devDependencies": { "@std/test": "^0.1.0" },
        "peerDependencies": {},
        "optionalDependencies": {},
        "overrides": {},
        "compilerOptions": { "strict": true },
        "tasks": { "build": "musi compile" },
        "fmt": { "useTabs": false, "lineWidth": 80, "indentWidth": 2 },
        "lint": { "report": "pretty", "rules": { "tags": ["recommended"] } },
        "test": { "include": ["**/*.test.ms"] },
        "bench": { "include": ["**/*.bench.ms"] },
        "compile": { "target": "x86_64-linux-gnu", "output": "./build/app" },
        "publish": { "include": ["src/**"] },
        "lock": { "path": "musi.lock", "frozen": true },
        "workspace": ["./a"],
        "exclude": ["vendor/"]
    }"#;
    let config = load_config_from_str(json).unwrap();
    assert_eq!(config.name.as_deref(), Some("@my/pkg"));
    assert_eq!(config.version.as_deref(), Some("2.0.0"));
    assert_eq!(config.description.as_deref(), Some("A test package"));
    assert_eq!(config.main.as_deref(), Some("src/app.ms"));
    assert!(config.exports.is_some());
    assert_eq!(config.license.as_deref(), Some("MIT"));
    assert!(config.author.is_some());
    assert!(config.contributors.is_some());
    assert_eq!(config.private, Some(true));
    assert!(config.repository.is_some());
    assert_eq!(config.homepage.as_deref(), Some("https://example.com"));
    assert!(config.bugs.is_some());
    assert_eq!(config.keywords.as_ref().map(Vec::len), Some(2));
    assert!(config.imports.is_some());
    assert!(config.scopes.is_some());
    assert!(config.dependencies.is_some());
    assert!(config.dev_dependencies.is_some());
    assert!(config.compiler_options.is_some());
    assert!(config.tasks.is_some());
    assert!(config.fmt.is_some());
    assert!(config.lint.is_some());
    assert!(config.test.is_some());
    assert!(config.bench.is_some());
    assert!(config.compile.is_some());
    assert!(config.publish.is_some());
    assert!(config.lock.is_some());
    assert!(config.workspace.is_some());
    assert!(config.exclude.is_some());
}

#[test]
fn author_as_string() {
    let json = r#"{ "author": "Alice <alice@example.com>" }"#;
    let config = load_config_from_str(json).unwrap();
    assert!(matches!(config.author, Some(Author::Inline(ref s)) if s.contains("Alice")));
}

#[test]
fn author_as_object() {
    let json = r#"{ "author": { "name": "Alice", "url": "https://alice.dev" } }"#;
    let config = load_config_from_str(json).unwrap();
    match config.author {
        Some(Author::Object(ref obj)) => {
            assert_eq!(obj.name, "Alice");
            assert_eq!(obj.url.as_deref(), Some("https://alice.dev"));
            assert!(obj.email.is_none());
        }
        _ => panic!("expected Author::Object"),
    }
}

#[test]
fn publish_as_false() {
    let json = r#"{ "publish": false }"#;
    let config = load_config_from_str(json).unwrap();
    assert!(matches!(
        config.publish,
        Some(PublishConfig::Disabled(false))
    ));
}

#[test]
fn lock_as_bool() {
    let json = r#"{ "lock": true }"#;
    let config = load_config_from_str(json).unwrap();
    assert!(matches!(config.lock, Some(LockConfig::Enabled(true))));
}

#[test]
fn lock_as_string() {
    let json = r#"{ "lock": "custom.lock" }"#;
    let config = load_config_from_str(json).unwrap();
    assert!(matches!(config.lock, Some(LockConfig::Path(ref p)) if p == "custom.lock"));
}

#[test]
fn fmt_config_parsing() {
    let json = r#"{
        "fmt": {
            "useTabs": true,
            "lineWidth": 120,
            "indentWidth": 4,
            "semiColons": false,
            "trailingCommas": "always",
            "bracePosition": "nextLine"
        }
    }"#;
    let config = load_config_from_str(json).unwrap();
    let fmt = config.fmt.unwrap();
    assert_eq!(fmt.use_tabs, Some(true));
    assert_eq!(fmt.line_width, Some(120));
    assert_eq!(fmt.indent_width, Some(4));
    assert_eq!(fmt.semi_colons, Some(false));
    assert_eq!(fmt.trailing_commas, Some(TrailingCommas::Always));
    assert_eq!(fmt.brace_position, Some(BracePosition::NextLine));
}

#[test]
fn unknown_fields_accepted() {
    let json = r#"{ "name": "test", "futureField": 42 }"#;
    let config = load_config_from_str(json).unwrap();
    assert_eq!(config.name.as_deref(), Some("test"));
}
