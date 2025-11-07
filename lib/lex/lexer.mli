(** Provides stateful lexer used to tokenise Musi source code. *)

(** Represents mutable lexer state with position tracking and diagnostics. *)
type t

(** Constructs lexer for given file contents and interner. *)
val make : Span.file_id -> string -> string -> Interner.t -> t

(** Tokenises entire source and returns tokens alongside diagnostics. *)
val lex : t -> Token.t list * Diagnostic.bag
