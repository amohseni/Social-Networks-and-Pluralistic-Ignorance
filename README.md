# Social networks and pluralistic ignorance: PI/FP data-generation lab

R implementation of the Truth-and-Conformity generative models for the
pluralistic-ignorance vs friendship-paradox identifiability project, with a
Shiny GUI for generating labeled survey data from each mechanism and their
mixtures.

Sources of truth (in the private for-claude repo): `models/pi-fp/design-2026-09-09.md`
(generators, experiment design) and `notes/2026-09-01-pi-vs-fp-identifiability.md`
(theory, tiers T0 to T4). The Python reference engine is
`models/pi-fp/tc_pi_engine.py`; `R/engine.R` is its port.

## Layout

- `R/engine.R`: topologies, scenarios S1/S2/S3, best-response dynamics. Base R only, deterministic per seed.
- `R/measure.R`: ground truth (three-term decomposition, PI signature flags), survey extraction, tier tables, edge-end estimator with bootstrap.
- `R/export.R`: per-run files (tier CSVs, `ground_truth.json`, `params.json`, `state.rds`, `trajectory.csv`).
- `R/sweep.R`: grid expansion and batch corpus production with `manifest.csv`; also a CLI.
- `app/app.R`: the Shiny GUI (sidebar controls, Run view, Data export, Batch, Schema tabs).
- `grids/`: YAML sweep grids (`corpus-v1.yaml` is the mechanism-square sweep).
- `tests/smoke_tests.R`: scenario acceptance tests; must print ALL PASS.
- `docs/`: proposals and the schema reference.

## Quick start

```bash
Rscript tests/smoke_tests.R
Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'
Rscript R/sweep.R grids/smoke.yaml corpora/smoke
```

Requires R 4.5 with shiny, bslib, igraph, ggplot2, DT, jsonlite, yaml.

## Tier file schema

Tiers are cumulative and each file is self-contained. `T0` to `T2` have one
row per ego; `T3`, `T3p` (T3 with alter degrees) and `T4` have one row per
(ego, alter slot), with alter slots shuffled so that no linkage exists below T4.

| File | Columns |
|---|---|
| T0.csv | ego_id, a, ghat |
| T1.csv | + d |
| T2.csv | + b |
| T3.csv | ego_id, alter_slot, a, ghat, d, b, ahat |
| T3p.csv | + alter_d |
| T4.csv | + alter_id, alter_a |

`a` attitude, `b` behavior (declaration), `d` degree, `ghat` perceived global
prevalence, `ahat` per-alter perception (face value of the alter's declaration).
