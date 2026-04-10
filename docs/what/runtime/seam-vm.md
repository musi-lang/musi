# SEAM VM

## Identity

SEAM VM executes typed SEAM programs on a stack-based machine.

It is not:

- generic machine assembly
- surface-syntax replay
- a second semantic checker

## Runtime Value Model

The runtime model includes:

- scalars
- strings
- sequences
- aggregates/data
- closures
- continuations
- type values
- module handles
- foreign handles

These values are runtime-level entities, not compiler-internal representations.

## Execution Model

Core execution pieces:

- loaded program metadata
- module initialization
- call frames
- operand stack
- globals
- active handlers and resumptions

Top-level execution still follows the language startup model through compiled entry/module-init behavior.

## Semantic Families

The VM owns runtime execution for:

- calls and closures
- sequence operations
- aggregate/data operations
- runtime type checks and casts
- resumable algebraic effects
- module loading
- foreign and host edges

## Host Boundary

Some seams remain host-owned by design:

- foreign call implementation
- unhandled host effects
- dynamic module program policy

Higher-level runtime services such as source-backed module loading and syntax compilation live above the VM in `musi_rt`.

The VM executes SEAM. The host decides external-world behavior.

## Boundaries

This document defines runtime semantics, not:

- byte encoding
- text assembly syntax
- project/package workflow

## See Also

- `docs/what/runtime/seam-bytecode.md`
- `docs/how/runtime/runtime-api.md`
- `docs/why/runtime-boundary.md`
