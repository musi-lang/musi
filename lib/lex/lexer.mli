open Basic

(** Lexer state *)
type t

(** Create lexer from source *)
val create : ?interner:Interner.t option -> Source.t -> int -> t

(** Tokenize entire source *)
val try_tokenize : t -> (Token.t * Span.t) list Reporter.result

(** Get next token *)
val try_next_token : t -> (Token.t * Span.t) Reporter.result

(** Get current lexer position *)
val curr_pos : t -> int

(** Get source being lexed *)
val source : t -> Source.t

(** Get file ID *)
val file_id : t -> int

(** Check if lexer encountered errors *)
val has_errors : t -> bool

(** Get diagnostics bag *)
val error_bag : t -> Reporter.bag

(** Print diagnostics to formatter *)
val emit_errors : Format.formatter -> t -> unit

(** Create token sequence option *)
val token_stream_opt : t -> (Token.t * Span.t) Seq.t option
