let log := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("log/index");
    testing.it("level 'name' renders known levels",
      testing.toBeTruthy(log.warnLevel.name() = "warn")
    );
    testing.it("level 'value' renders numeric level",
      testing.toBe(log.errorLevel.value(), 40)
    );
    testing.it("'write' returns without failure",
        (
          log.write(log.errorLevel, "boom");
          0 = 0
        )
    );
    testing.it("'debug' returns without failure",
        (
          log.debug("d");
          0 = 0
        ));
    testing.it("'info' returns without failure",
        (
          log.info("i");
          0 = 0
        ));
    testing.endDescribe()
  );
