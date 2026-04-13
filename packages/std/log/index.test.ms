let Testing := import "@std/testing";
let Log := import "./index.ms";

export let test () :=
  (
    Testing.describe("log/index");
    Testing.it("'levelName' renders known levels", Testing.toBeTruthy(Log.levelName(Log.warnLevel) = "warn"));
    Testing.it("'write' returns without failure", (Log.write(Log.errorLevel, "boom"); 0 = 0));
    Testing.it("'debug' returns without failure", (Log.debug("d"); 0 = 0));
    Testing.it("'info' returns without failure", (Log.info("i"); 0 = 0));
    Testing.endDescribe()
  );
