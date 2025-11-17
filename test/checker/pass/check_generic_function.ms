val identity<T> := fn (x: T) -> T { x };

val int_result := identity<Int>(42);
val str_result := identity<Str>("hello");
