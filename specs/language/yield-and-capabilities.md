# Yield And Capability Objects

Status: frozen 0.1.0 host-language baseline

Musi small core uses capability objects for authority and `yield` for the only primitive suspension point.

## Suspension

`yield` is the only primitive suspension point.

```musi
let reply := yield request;
```

Meaning:

- suspend current coroutine frame,
- emit `request` to its driver,
- continue later with `reply`.

Task, scheduler, async, await, and spawn behavior belongs to protocol and library values. Scheduler authority must be carried by an explicit capability object.

## Capabilities

Capability objects are ordinary values that carry authority. No object means no authority.

```musi
let Logger := shape {
  let write(level : LogLevel, text : String) : IOError!();
};

let run(log : erased Logger) : IOError!() := (
  log.write(.Info, "starting")
);
```

Capability objects model IO, logging, time, randomness, filesystem access, networking, host services, sandbox permissions, stateful services, and schedulers.

## Function Types

`A -> B` is function type syntax. If a computation can suspend or fail, that consequence must be visible in its protocol or return type.
