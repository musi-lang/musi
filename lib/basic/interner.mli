(** Provides string interner for efficient identifier and literal storage. *)

(** Represents table storing interned strings. *)
type t

(** Represents unique identifier assigned to interned string. *)
type name = private int

(** Constructs new empty interner. *)
val create : unit -> t

(** Returns identifier for given string, interning it if necessary. *)
val intern : t -> string -> name

(** Recovers original string associated with identifier. *)
val lookup : t -> name -> string

(** Orders two interned names by numeric identifiers. *)
val compare : name -> name -> int

(** Checks whether two interned names refer to same string. *)
val equal : name -> name -> bool
