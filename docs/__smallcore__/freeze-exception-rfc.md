# Freeze-Exception RFC Process

Status: `active`
Policy source: `docs/__smallcore__/freeze-policy.md`
Ledger reference: `docs/where/bootstrap-bytecode-ledger.md`
Checkpoint log: `docs/__smallcore__/checkpoint-log.md`

## When RFC Is Required

Open this RFC flow before merge when a change:

- modifies meaning of frozen language/SEAM contracts;
- adds, removes, or renames any canonical file tracked by freeze manifest;
- changes lowering/runtime/artifact rules that alter frozen guarantees.

## Required Submission Artifact

Every freeze-exception proposal must include one committed RFC note with:

1. **Change summary** — one paragraph with the exact frozen rule/surface affected.
2. **Canonical diff map** — file list with before/after ownership and semantics.
3. **Compatibility impact** — parser/lowering/runtime/artifact/tooling consequences.
4. **Risk and rollback** — failure modes plus rollback plan.
5. **Validation evidence** — commands and logs, including:
   - `rtk cargo test -p musi --test freeze_contract`
   - any additional subsystem gates used for the proposal.
6. **Approval block** — named reviewers and final decision date.

## Approval Gates

Required reviewers:

- syntax reviewer;
- lowering reviewer;
- runtime reviewer;
- artifacts reviewer;
- tooling reviewer;
- roadmap owner.

Decision states: `approved`, `rejected`, `superseded`.

## Merge Conditions

A freeze-exception change is merge-ready only when:

- RFC decision state is `approved`;
- `docs/__smallcore__/freeze-manifest.toml` is updated to the approved canonical set;
- checkpoint log includes command evidence path and RFC link;
- `rtk cargo test -p musi --test freeze_contract` passes on current branch.
