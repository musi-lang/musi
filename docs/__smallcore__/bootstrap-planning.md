# Bootstrap Planning

Status: `active`
Roadmap link: `docs/__smallcore__/PLAN.md`
Compiler bootstrap roadmap: `docs/where/compiler-bootstrap-roadmap.md`
Decision log: `docs/__smallcore__/decisions/2026-05-14-roadmap-kickoff.md`
Checkpoint log: `docs/__smallcore__/checkpoint-log.md`

## Scope Boundaries

- Bootstrap work must conform to frozen small-core language and SEAM contracts.
- Bootstrap work must not redefine frozen syntax, lowering semantics, runtime contracts, or artifact policy.
- Proposed exceptions require freeze-exception RFC approval.

## Milestones

- M1: bootstrap syntax path validated against frozen grammar contracts.
- M2: bootstrap lowering path validated against frozen SEAM semantics.
- M3: bootstrap runtime path validated against frozen runtime and artifact contracts.
- M4: bootstrap tooling path validated for build, disasm, and artifact inspection flows.

## Validation Gates

- Gate 1: syntax reviewer sign-off.
- Gate 2: lowering reviewer sign-off.
- Gate 3: runtime reviewer sign-off.
- Gate 4: artifacts reviewer sign-off.
- Gate 5: tooling reviewer sign-off.

## Compiler-side phase trackers

- P00 syntax contracts: `docs/where/compiler-bootstrap-phase-00-syntax-contracts.md`
- P01 module/name graph: `docs/where/compiler-bootstrap-phase-01-module-name-graph.md`
