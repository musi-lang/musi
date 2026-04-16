let testing := import "@std/testing";
let log := import "./index.ms";

export let test () :=
  (
    testing.describe("log/index");
    let warn := log.warnLevel();
    let error := log.errorLevel();
    testing.it("'levelName' renders known levels", testing.toBeTruthy(log.levelName(warn) = "warn"));
    testing.it("'levelValue' renders numeric level", testing.toBe(log.levelValue(error), 40));
    testing.it("'write' returns without failure", (log.write(error, "boom"); 0 = 0));
    testing.it("'debug' returns without failure", (log.debug("d"); 0 = 0));
    testing.it("'info' returns without failure", (log.info("i"); 0 = 0));
    testing.endDescribe()
  );
