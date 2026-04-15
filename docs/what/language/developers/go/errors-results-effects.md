---
title: "Errors, Results, and Effects"
description: "Translate Go explicit error returns and host interaction into Musi Result values and effect boundaries."
group: "Musi for Developers"
section: "Go Developers"
order: 8
slug: "errors-results-effects"
summary: "Keep expected failure as data and outside work behind effectful APIs."
---

# Errors, Results, and Effects

Go makes expected failure explicit with `error`:

```go
func parsePort(text string) (int, error) {
    if text == "8080" {
        return 8080, nil
    }
    return 0, errors.New("parse error")
}

port, err := parsePort("abc")
if err != nil {
    port = 3000
}
```

Musi keeps expected failure in the returned value.

{{snippet:go-errors-results}}

## Outside work

Go standard input and output use packages directly:

```go
reader := bufio.NewReader(os.Stdin)
name, _ := reader.ReadString('\n')
fmt.Println(strings.TrimSpace(name))
```

Musi keeps console work at effectful boundaries.

{{snippet:go-effect-boundary}}
