# Proposal: data-generation GUI, tier schema, and sweep grid

Date: 2026-09-10. Status: **superseded in part on 2026-09-10: Aydin chose R Shiny over Streamlit and approved building.** What was built the same day: `R/engine.R` (port of the Python engine, plus extensions off by default), `R/measure.R`, `R/export.R`, `R/sweep.R`, `app/app.R` (Shiny), `tests/smoke_tests.R` (ALL PASS), `grids/corpus-v1.yaml`. The tier schema of §2.1 is implemented as written. Sections 2 (wireframe) and 3 (grid) remain the reference; §6 questions 1, 2 and 7 are settled (this repo; R Shiny; schema and batch built with the GUI), the rest are still open. Original status line: proposal, awaiting Aydin's approval before any build. Written by the Claude Code session continuing the 2026-09-10 handoff. Sources of truth: `for-claude/models/pi-fp/design-2026-09-09.md` (generators, experiment design) and `for-claude/notes/2026-09-01-pi-vs-fp-identifiability.md` (theory, tiers). Nothing below re-derives either; section references point at them.

Typing note. Sections 2 to 4 are design proposals (artifacts to be built), not claims. Section 5 restates preregistered expectations (predictions, from the design doc §4) with the concrete pass criteria the battery will apply. Section 6 is the list of decisions.

## 0. Session state (verified on this Mac, 2026-09-10)

- for-claude clone found at `/Users/amohseni/Documents/GitHub/for-claude`, not at the path the handoff and `CLAUDE.md` give (`/Users/amohseni/Claude/for Claude`, which does not exist). The clone was one commit behind `origin/main`; fast-forwarded locally to `6c39db1` (the auto-sync commit carrying `models/pi-fp/` and the thread 3 entry). The handoff's hash `684c252` does not exist in this clone; `6c39db1` is the commit that delivered the files.
- `smoke_tests.py`: ALL PASS (Python 3.12.5, numpy 2.5.3, in a scratch venv; no system Python here had numpy).
- `figure_mechanism_square.py` regenerated in the scratchpad: per-class PI counts 4/60, 14/60, 46/60, 35/60, 60/60, identical to the counts recorded in thread 3. The engine is deterministic across machines.
- Sync agent: no `.sync-heartbeat` in the clone and no `for-claude` job in `launchctl list`. Per the repo contract the agent should be treated as down. See question 6.
- Per-run engine cost (single core, this Mac):

| n | S1 er / ba | S2-mixed ba | S2-pure ba | S3 regular k=8 | S3 complete |
|---|---|---|---|---|---|
| 200 | 9 / 4 ms | 2 ms | 1 ms | 3 ms | 10 ms |
| 500 | 10 / 10 ms | 8 ms | 3 ms | 7 ms | 54 ms |
| 1000 | 27 / 22 ms | 14 ms | 6 ms | 23 ms | not run |

All benchmark runs converged in under 10 rounds. Compute is not the binding constraint on corpus size; disk and I/O for tier files are.

## 1. Repository layout (proposal)

Use the working repo `Social-Networks-and-Pluralistic-Ignorance` (currently only a LICENSE) as the standalone codebase the handoff calls `pi-fp-lab`. for-claude keeps state only, with a pointer note left in `models/pi-fp/`.

```
pifp/                      importable package
  engine.py                tc_pi_engine.py, vendored unchanged (GUI-free, numpy only)
  schema.py                tier column definitions, one place
  export.py                State -> tier CSVs + ground_truth.json (+ state.npz)
  batch.py                 grid spec -> corpus dir + manifest.csv
  analyze/
    tiers.py               T0..T4 tests (one function per tier, each cites its proposition)
    classify.py            cross-validated logistic + gradient boosting, AUC with CIs
    decomposition.py       estimator evaluation against ground truth (bias, RMSE, coverage)
    report.py              per-tier verdicts -> markdown/JSON report
  cli.py                   pifp generate | pifp sweep | pifp analyze <corpus-dir>
app/streamlit_app.py       thin layer over pifp
tests/                     smoke_tests.py moved here unchanged, plus schema and estimator tests
grids/                     YAML sweep grids (the ones in section 3)
corpora/                   generated data, gitignored
docs/                      this proposal, later the schema reference
```

Dependencies: numpy, pandas, streamlit, networkx (drawing only), matplotlib, scikit-learn, pyyaml. Engine keeps zero dependencies beyond numpy.

## 2. GUI wireframe (Streamlit v1)

Layout: a persistent left sidebar holds the generator controls; the main area has four tabs. The sidebar state feeds tabs 1 and 2 directly; tabs 3 and 4 have their own inputs.

**Sidebar: generator controls**

| Control | Widget | Default | Shown for |
|---|---|---|---|
| Scenario | radio: S1, S2-pure, S2-mixed, S3 | S1 | always |
| Topology | select: complete, ring, regular, er, ba, star, core_periphery | er | always |
| Topology parameter | regular: k (even); er: mean degree (converted to p_edge); ba: m; core_periphery: s, k_out | k=4, mean degree 6, m=2, s=n/20, k_out=1 | by topology |
| n | number | 200 | always |
| pi_0 | slider 0.5 to 0.99 | 0.6 | S1, S2 |
| F_alpha | select: uniform, beta(a,b), point(alpha_0); a, b or alpha_0 inputs appear | uniform | S1, S2-mixed, S3 |
| Credence source | select: signal (k signals), uniform | signal, k=1 | always |
| lambda | slider 0 to 10 | 4 | S2 |
| psi | slider 0.5 to 1 | 0.7 | S3 |
| Seed | number | 0 | always |
| Replicates | number 1 to 50 | 1 | always |

Vocabulary on screen follows design doc §1 and §3: attitude, declaration, credence, type, misperception term, structure term, PI signature.

**Tab 1: Run view** (replicate 1 in detail, all replicates in the summary table)

1. Network drawing, shown when n <= 300: node fill = attitude, node ring = declaration, hub-aware spring layout, legend in the corner. Above 300 nodes the panel says so and shows the degree histogram instead.
2. Decomposition bar: gap = misperception term + structure term, majority-attitude frame (same sign convention as `figure_mechanism_square.py`), with the residual printed (should be 0 to machine precision).
3. Mechanism square: this run's replicates as large markers over a cached background cloud (the 300 runs of the current diagnostic figure, cached to disk keyed by an engine hash so it regenerates when the engine changes).
4. S3 only: declaration trajectory, mean declaration per round, with the tipping/persistence outcome labeled.
5. Convergence and ground-truth table: converged, rounds, p, mean ĝ, p̃, census q̂, degree-weighted net falsification, falsified share, PI signature, PI anchor.
6. Replicate summary table: one row per replicate with the same columns, plus class frequencies (PI signature share).

**Tab 2: Data export**

Inputs: ego sample size m (default min(100, n)), survey seed, tiers to include (all checked). Button writes a zip: `run_<seed>/params.json`, `ground_truth.json`, `state.npz`, `T0.csv` ... `T4.csv`. Schema (section 2.1) shown in an expander.

**Tab 3: Batch**

Inputs: grid preset (mechanism-square sweep v1; matched-T0 calibration; or upload a YAML grid), seeds per cell, n override, m list, output directory. Runs with a progress bar, writes `manifest.csv` (one row per run: parameters, seed, label, convergence, ground-truth terms, PI flags, file paths). Same code path as `pifp sweep`, so the CLI and the GUI produce byte-identical corpora for the same grid and seeds.

**Tab 4: Analyze**

Input: corpus directory. Calls `pifp analyze` and renders the per-tier report (section 4). Built after the battery exists; included in the wireframe so the layout is settled once.

### 2.1 Tier file schema (stable; the battery consumes exactly these)

Tiers are cumulative, as in the note §6. Files are self-contained CSVs so R users load one file per tier.

| File | Row unit | Columns |
|---|---|---|
| T0.csv | ego | ego_id, a, ghat |
| T1.csv | ego | + d |
| T2.csv | ego | + b |
| T3.csv | (ego, alter slot) | ego_id, alter_slot, a, ghat, d, b, ahat |
| T3p.csv | (ego, alter slot) | + alter_d |
| T4.csv | (ego, alter slot) | + alter_id, alter_a |
| ground_truth.json | run | every field of `ground_truth()` plus n, topology, degree summary, convergence |
| params.json | run | scenario, topology and parameters, n, pi_0, F_alpha, credence, lambda or psi, seed, m, survey seed |
| state.npz | run | a, D, c, alpha, edge list (lets any m or tier be resampled later; small at n <= 2000) |

Alter slot is the position in the ego's neighbor list, shuffled per ego, so T3 carries no linkage. T3' is spelled `T3p` in file names. Values are 0/1 integers except ghat (float) and ids.

## 3. Sweep grid (corpus v1, mechanism-square sweep)

Base n = 500. Seeds per cell: 50. m list per run: 50, 100, 200, census (the tier files are cheap; state.npz allows any other m later).

| Class | Topologies | pi_0 | F_alpha | Mechanism dial | Cells |
|---|---|---|---|---|---|
| S1 | er (mean degree 4, 8), ba (m=2, 4), regular (k=4, 8) | 0.55, 0.6, 0.7, 0.8 | U[0,1], Beta(1,3), Beta(1,6), Beta(3,1) | none | 96 |
| S2-pure | ba (m=1, 2, 4), core_periphery (s=10 k_out=1; s=25 k_out=2), er (mean degree 4), regular (k=8, control) | 0.6, 0.7, 0.8, 0.9 | alpha = 1 | lambda 0, 0.5, 1, 2, 4, 8 | 168 |
| S2-mixed | same seven | 0.6, 0.7, 0.8 | U[0,1], Beta(1,3), Beta(3,1) | lambda 0, 1, 2, 4, 8 | 315 |
| S3 | complete, regular (k=4, 8), er (mean degree 8), ba (m=2, 4) | (p = 1 - psi) | U[0,1], Beta(1,3), Beta(1,6) | psi 0.55, 0.6, 0.7, 0.8, 0.9 | 90 |

Totals: 669 cells, about 33,000 runs, roughly 6 minutes of engine time single-core at n = 500, plus tier-file I/O (estimated 10 to 20 minutes; measured before the full run). Credence source: the design doc default (signal, k = 1) throughout; the uniform alternative enters only if question 3.3 says so.

Coverage of the square, by construction (design doc §4.1): S2-pure moves along the structure axis at exactly zero misperception; S3 and S1 on regular topologies move along the misperception axis at exactly zero structure (Proposition 2); S2-mixed and S1 on er and ba fill the interior. The regular-graph rows of S2-pure are controls (structure term forced to 0) and are expected to show no PI signature.

Power subgrid (design doc §4.3 iii): a reduced grid (S1 er mean degree 8 with Beta(1,3); S2-pure and S2-mixed ba m=2 with lambda 0, 2, 8; S3 regular k=8 with psi 0.6, 0.8), 50 seeds, at n = 200, 1000, 2000. About 40 cells times 3 sizes, 6,000 runs.

Matched-T0 pairs (design doc §4.2): bin every run by (p, mean ĝ) in 0.05 by 0.05 cells; keep cells with at least 20 runs from each of at least two classes. Matching on p is exact by construction in S2 (pi_0) and S3 (1 - psi) and binomial in S1, so the calibration axis is mean ĝ. If a target cell (p in {0.6, 0.7, 0.8}; mean ĝ in {0.30, 0.40, 0.45}) is under-populated for some class after the sweep, a calibration search adjusts that class's mechanism dial (lambda for S2, F_alpha for S3 and S1) by bisection on the ensemble mean ĝ. The sweep is run first; the search is only for cells the grid misses.

Convergence: every run records rounds and the converged flag; non-converged runs stay in the corpus, flagged, and are excluded from the acceptance tests with the count reported.

## 4. Test battery (summary; the full spec follows approval of this proposal)

Per tier, one function that takes the tier file(s) and returns a verdict record. Each cites the proposition it implements.

| Tier | Test | Proposition |
|---|---|---|
| T0 | p̂, mean ĝ, gap; generic classifier at chance on matched cells | 1 |
| T1 | Cov(a, d)/mean(d) estimate of p̃ - p; Proposition 3 audits (near-regular degrees with anchor signature and p̂ > 3/4 refutes structure-alone; sigma bound check); verdict is one-sided: "structure-alone refuted" or "not refuted", never "established" | 2, 3 |
| T2 | P̂(b=0 mid a=1), P̂(b=1 mid a=0) with Wilson CIs; degree-weighted falsified share | note §6 T2 |
| T3 | q̂ - p̃̂ with ego-level bootstrap CI; under the configuration-model benchmark, all three decomposition terms; degree-assortativity caveat printed when T3p is absent | 4 |
| T3' | reach-based structure term from alter degrees; assortativity measured | note §6 refinement |
| T4 | dyad-level misperception rates by direction; benchmark truth | note §6 T4 |

Generic discrimination: per tier, cross-validated logistic regression and gradient boosting on tier-computable summaries (T0: p̂, mean ĝ, sd ĝ, mean ĝ by attitude; T1 adds degree moments, Cov(a, d), sigma-hat, budget slack; T2 adds the falsification estimates; T3 adds q̂ - p̃̂; T3' adds assortativity and the reach estimate; T4 adds dyad rates). Label: mechanism class (S1-PI, S1-noPI, S2-pure, S2-mixed, S3-persist, S3-tip). AUC with stratified-bootstrap CIs. Mixtures (S2-mixed, S1 on heterogeneous graphs): decomposition estimators scored against the ground-truth terms (bias, RMSE, 95 percent CI coverage), not labels.

## 5. Preregistered expectations with pass criteria (from design doc §4.4)

1. T0: AUC within matched-T0 cells in [0.45, 0.55], CI covering 0.5. This is Proposition 1 made empirical and is a positive result.
2. T1: on regular-graph S3-persist and S1-PI runs with p > 3/4 and the anchor signature, the structure-alone refutation fires in 100 percent of runs; on S2-pure runs it never fires (a firing there is a bug in the audit or in the theory, reported either way).
3. T3: q̂ - p̃̂ recovers the degree-weighted net falsification within sampling error (census: exact, as smoke tests E1 and E3 show); coverage of the bootstrap CI near nominal at m = 100 and m = 200; bias grows with measured degree assortativity when T3p is absent.
4. AUC weakly increases along T0 < T1 < T2 < T3 < T3' < T4.

Deviations are reported as bugs or findings; none is silently corrected.

## 6. Decisions needed from Aydin (one list)

1. Repo: build in `Social-Networks-and-Pluralistic-Ignorance` with the layout of section 1, engine vendored unchanged, pointer note left in for-claude `models/pi-fp/`? (Recommended.)
2. Stack: Streamlit v1 with the sidebar-plus-four-tabs wireframe of section 2; R/Shiny port deferred. Confirm the wireframe or mark panels to change.
3. Design doc §6, still open; my recommendation in brackets: (3.1) frozen beliefs default, internalization as a later dial [confirm]; (3.2) 1.1/1.2 as ex-post outcome classes of S1 [confirm]; (3.3) credence from the signal structure by default, uniform as a dial [signal]; (3.4) ĝ = f̂ with no elicitation noise by default [confirm; noise as a robustness dial in the batch grid later]; (3.5) Python engine now, R port when the design settles [Python]; (3.6) eigenvector-centrality variant of S2 [not in corpus v1; degree only].
4. Corpus v1 scale: section 3 grid, n = 500, 50 seeds per cell, about 33,000 runs plus the 6,000-run power subgrid. Larger or smaller?
5. Tier schema of section 2.1 (cumulative, self-contained CSVs, long format for T3 to T4, state.npz per run): confirm, since the battery is written against it.
6. Sync and paths: the for-claude clone is at `Documents/GitHub/for-claude`, `CLAUDE.md` names a different path, and no sync agent is running here. Should this session repair the launchd agent per the contract, or is the Mac clone now synced another way? Until settled, write-back receipts from this session are file paths in that clone, uncommitted.
7. Build order (recommended): schema and export, batch and CLI, test battery, then GUI tabs 1 to 3, then tab 4. The GUI is the handoff's target A, but the battery and the GUI both consume the schema, so the schema goes first either way.
