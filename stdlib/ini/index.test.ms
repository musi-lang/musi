//! Tests for INI parse and get operations.

import "@std/testing" as t;
import "@std/ini/index" as ini;

let simple_ini := "[server]\nhost = localhost\nport = 8080\n\n[db]\nname = mydb\n";

t.describe("INI", () => (
  t.it("parse - empty string returns empty sections", () => (
    let sections := ini.parse("");
    t.expect(sections).toBe([])
  ));
  t.it("parse - single section with entries", () => (
    let sections := ini.parse(simple_ini);
    t.expect(sections.[0].name).toBe("server")
  ));
  t.it("parse - multiple sections", () => (
    let sections := ini.parse(simple_ini);
    t.expect(sections.[1].name).toBe("db")
  ));
  t.it("get - finds value in section", () => (
    let sections := ini.parse(simple_ini);
    t.expect(ini.get(sections, "server", "host")).toBe(.Some("localhost"))
  ));
  t.it("get - finds numeric value", () => (
    let sections := ini.parse(simple_ini);
    t.expect(ini.get(sections, "server", "port")).toBe(.Some("8080"))
  ));
  t.it("get - returns None for missing key", () => (
    let sections := ini.parse(simple_ini);
    t.expect(ini.get(sections, "server", "password")).toBe(.None)
  ));
  t.it("get - returns None for missing section", () => (
    let sections := ini.parse(simple_ini);
    t.expect(ini.get(sections, "cache", "host")).toBe(.None)
  ));
  t.it("parse - semicolon comment lines ignored", () => (
    let input := "; this is a comment\n[app]\nkey = value\n";
    let sections := ini.parse(input);
    t.expect(sections.[0].name).toBe("app")
  ));
  t.it("parse - hash comment lines ignored", () => (
    let input := "# another comment\n[app]\nkey = value\n";
    let sections := ini.parse(input);
    t.expect(ini.get(sections, "app", "key")).toBe(.Some("value"))
  ));
  t.it("parse - keys with spaces around equals", () => (
    let input := "[s]\nfoo = bar\n";
    let sections := ini.parse(input);
    t.expect(ini.get(sections, "s", "foo")).toBe(.Some("bar"))
  ));
  t.it("parse - global entries before any section", () => (
    let input := "key = value\n[s]\nother = x\n";
    let sections := ini.parse(input);
    t.expect(ini.get(sections, "", "key")).toBe(.Some("value"))
  ));
  t.it("parse - section with multiple entries", () => (
    let input := "[s]\na = 1\nb = 2\nc = 3\n";
    let sections := ini.parse(input);
    t.expect(
      ini.get(sections, "s", "a") = .Some("1") and
      ini.get(sections, "s", "b") = .Some("2") and
      ini.get(sections, "s", "c") = .Some("3")
    ).toBe(.True)
  ))
));

t.run();
