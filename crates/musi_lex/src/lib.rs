//! Lexer for the Musi compiler.
//!
//! Converts a source string into a flat stream of [`Token`]s.
//! The lexer is a hand-written, single-pass [`Iterator`] that never allocates
//! except to intern string data into the caller-supplied [`Interner`].
//!
//! # Error handling
//!
//! Lexical errors (unknown characters, unterminated strings, etc.) are recorded
//! into the caller-supplied [`DiagnosticBag`] and produce an [`TokenKind::Error`]
//! token so that the rest of the stream remains valid.

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

pub mod lexer;
pub mod token;

pub use lexer::Lexer;
pub use token::{Token, TokenKind};
