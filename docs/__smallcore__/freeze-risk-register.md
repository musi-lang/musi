# Freeze Risk Register

Status: `open for post-freeze monitoring`
Cadence: `biweekly`
Checkpoint log: `docs/__smallcore__/checkpoint-log.md`
Decision log: `docs/__smallcore__/decisions/2026-05-14-roadmap-kickoff.md`

## Active Risks

| ID    | Risk                                               | Owner Role                           | Mitigation                                                  | Status     |
| ----- | -------------------------------------------------- | ------------------------------------ | ----------------------------------------------------------- | ---------- |
| FR-01 | Frozen syntax rule drift in implementation updates | syntax reviewer                      | Require grammar alignment check on syntax-affecting changes | monitoring |
| FR-02 | Lowering/runtime semantic divergence               | lowering reviewer + runtime reviewer | Require cross-doc matrix check before merge                 | monitoring |
| FR-03 | Artifact format policy drift (`.seam`/`.mar`)      | artifacts reviewer                   | Require artifact compatibility review in release gates      | monitoring |
| FR-04 | CLI/tooling drift from frozen contracts            | tooling reviewer                     | Require CLI regression gate for affected commands           | monitoring |
