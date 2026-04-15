---
title: "Goroutines, Channels, Context, and Effects"
description: "Translate Go concurrency habits into Musi effect boundaries and handlers."
group: "Musi for Developers"
section: "Go Developers"
order: 9
slug: "goroutines-channels-context-effects"
summary: "Use effects to name outside work and handler boundaries to decide how it runs."
---

# Goroutines, Channels, Context, and Effects

Go concurrency often starts a goroutine, sends through a channel, and waits for a value:

```go
func loadPort() int {
    done := make(chan int, 1)
    go func() {
        done <- 8080
    }()
    return <-done
}
```

Musi does not hide outside work behind a goroutine marker in ordinary expressions. Name the operation as an effect request, then decide how it is interpreted at the boundary.

{{snippet:go-effect-request}}

Go `context.Context` carries cancellation and deadlines through call chains:

```go
func readPort(ctx context.Context) (int, error) {
    select {
    case <-ctx.Done():
        return 0, ctx.Err()
    default:
        return 8080, nil
    }
}
```

In Musi, pass ordinary data as data and keep cancellation, scheduling, or host interaction in the effect surface that the handler owns.

{{snippet:go-handler-boundary}}
