val identity<T> := fn (x: T) -> T { x };

val int_result := identity(42);
val str_result := identity("hello");
