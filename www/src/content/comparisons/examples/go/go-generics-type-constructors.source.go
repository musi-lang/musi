type Box[T any] struct {
    Value T
}

boxedName := Box[string]{Value: "Nora"}
