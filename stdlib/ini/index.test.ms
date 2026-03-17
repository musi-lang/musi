//! Tests for INI parse and get operations.

import "@std/testing" as t;
import "@std/ini/index" as ini;

let simple_ini := "[server]\nhost = localhost\nport = 8080\n\n[db]\nname = mydb\n";

t.runner.report(t.runner.run_suite([
  ("parse - empty string returns empty sections", () => (
    let sections := ini.parse("");
    t.assertions.assert_eq(sections, [])
  )),
  ("parse - single section with entries", () => (
    let sections := ini.parse(simple_ini);
    t.assertions.assert_eq(sections.[0].name, "server")
  )),
  ("parse - multiple sections", () => (
    let sections := ini.parse(simple_ini);
    t.assertions.assert_eq(sections.[1].name, "db")
  )),
  ("get - finds value in section", () => (
    let sections := ini.parse(simple_ini);
    t.assertions.assert_eq(ini.get(sections, "server", "host"), .Some("localhost"))
  )),
  ("get - finds numeric value", () => (
    let sections := ini.parse(simple_ini);
    t.assertions.assert_eq(ini.get(sections, "server", "port"), .Some("8080"))
  )),
  ("get - returns None for missing key", () => (
    let sections := ini.parse(simple_ini);
    t.assertions.assert_eq(ini.get(sections, "server", "password"), .None)
  )),
  ("get - returns None for missing section", () => (
    let sections := ini.parse(simple_ini);
    t.assertions.assert_eq(ini.get(sections, "cache", "host"), .None)
  )),
  ("parse - semicolon comment lines ignored", () => (
    let input := "; this is a comment\n[app]\nkey = value\n";
    let sections := ini.parse(input);
    t.assertions.assert_eq(sections.[0].name, "app")
  )),
  ("parse - hash comment lines ignored", () => (
    let input := "# another comment\n[app]\nkey = value\n";
    let sections := ini.parse(input);
    t.assertions.assert_eq(ini.get(sections, "app", "key"), .Some("value"))
  )),
  ("parse - keys with spaces around equals", () => (
    let input := "[s]\nfoo = bar\n";
    let sections := ini.parse(input);
    t.assertions.assert_eq(ini.get(sections, "s", "foo"), .Some("bar"))
  )),
  ("parse - global entries before any section", () => (
    let input := "key = value\n[s]\nother = x\n";
    let sections := ini.parse(input);
    t.assertions.assert_eq(ini.get(sections, "", "key"), .Some("value"))
  )),
  ("parse - section with multiple entries", () => (
    let input := "[s]\na = 1\nb = 2\nc = 3\n";
    let sections := ini.parse(input);
    t.assertions.assert_true(
      ini.get(sections, "s", "a") = .Some("1") and
      ini.get(sections, "s", "b") = .Some("2") and
      ini.get(sections, "s", "c") = .Some("3")
    )
  )),
]));
