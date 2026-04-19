let testing := import "@std/testing";

export let test () :=
  testing.it("adds receipt lines", testing.toBe(450 + 120, 570));
