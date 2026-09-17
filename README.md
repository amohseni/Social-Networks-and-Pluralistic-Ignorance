# Social networks and pluralistic ignorance: PI/FP data-generation lab

R implementation of the generative models for the pluralistic-ignorance vs
friendship-paradox identifiability project, with a Shiny GUI for generating
labeled survey data from each mechanism and their mixtures.

Model: agents on an undirected network with private attitude A_i, public
declaration D_i and conformity parameter c_i; declaring D pays
c_i N_i(D) + (1 - c_i) 1[D = A_i]; asynchronous best response in random
order until a quiet round. Pluralistic ignorance is a property of an individual agent: it holds the
majority attitude, believes that a majority of its neighbors hold the opposite
attitude, that belief is mistaken, and it declares the opposite attitude. The
prevalence of pluralistic ignorance is the share of agents in that state.

Sources of truth (in the private for-claude repo): `models/pi-fp/design-2026-09-09.md`
(generators, experiment design) and `notes/2026-09-01-pi-vs-fp-identifiability.md`
(theory, data tiers). The Python engine in `models/pi-fp/tc_pi_engine.py` was
the first draft; the R engine now implements the 2026-09-11 model and is the
reference.

## Layout

- `R/engine.R`: topologies, scenarios S1/S2/S3, best-response dynamics. Base R plus igraph for eigenvector and betweenness centrality; deterministic per seed.
- `R/measure.R`: ground truth (two-term decomposition, pluralistic-ignorance verdict), survey extraction, survey designs, edge-end estimator with bootstrap.
- `R/export.R`: per-run files (`respondents.csv`, `ego_network.csv`, `ground_truth.json`, `params.json`, `state.rds`, `trajectory.csv`).
- `R/sweep.R`: research sweeps over parameter grids with `manifest.csv` (CLI only).
- `R/corpus.R`: the blinded corpus (datasets that all show the pattern, from every scenario, with a key kept aside) and the scorer for analysts' predictions; also a CLI.
- `app.R`: the Shiny GUI (Generate corpus, Explore, Score predictions, Guide). Self-contained with `R/`; every output is a download. `R/theme.R` holds the visual language. `deploy.R` publishes it to shinyapps.io. `app/theme.R` holds the visual language shared with Aydin's other GUIs: bootswatch 3 Paper chrome and the ggplot2 palette orangered2 / #3475BC / black at text size 16.
- `grids/`: YAML sweep grids (`corpus-v1.yaml` is the mechanism-square sweep).
- `tests/smoke_tests.R`: scenario acceptance tests; must print ALL PASS.
- `docs/`: proposals and the schema reference.

## Quick start

```bash
Rscript tests/smoke_tests.R
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
Rscript R/corpus.R corpora/blinded-v1 --per 40 --per-variants 20 --respondents 150 --seed 1
```

Requires R 4.5 with shiny, bslib, igraph, ggplot2, DT, jsonlite, yaml, zip.

Published app: https://amohseni.shinyapps.io/Social-Networks-and-Pluralistic-Ignorance/ (deploy with `Rscript deploy.R`; see the file for the one-time account setup).

## The generated corpus

`generate_blinded_corpus()` (Generate corpus tab, or the CLI above) writes
`<out>/blinded/` for the analysts (README.txt, respondents.csv, ego_network.csv,
all datasets stacked and identified by dataset_id) and, for the person running
the test, `<out>/key.csv` plus `<out>/key_details/nodes.csv` and `edges.csv`
(every agent and tie of every population). Everything is CSV.
Pluralistic ignorance obtains in every dataset, in the population and in the
sample. The process is genuine pluralistic ignorance (random start, private
change of mind) or the friendship paradox (well-connected minority with
conformity off). Parameters are drawn at random for each dataset; all datasets
use preferential-attachment networks; the attitude coding is reversed in a
random half of the datasets; respondent identifiers and dataset numbers are
assigned at random. `score_predictions()` compares a predictions CSV
(dataset_id, prediction in {genuine, structure}, optional score) with the key.

## Exported data

Each run exports a uniform sample of respondents. The survey design chooses
which columns are recorded; attitude and perceived prevalence are always
present.

| File | Columns | Present when |
|---|---|---|
| respondents.csv | respondent_id, attitude, perceived_prevalence | always |
| | + degree | number of contacts recorded |
| | + declaration | own declaration recorded |
| ego_network.csv | respondent_id, neighbor_slot, perceived_neighbor_attitude | contact perceptions recorded (contacts anonymous, slots shuffled) |
| | + neighbor_degree | contacts' numbers of contacts recorded |
| | + neighbor_id, neighbor_attitude | linked design |
| ground_truth.json, params.json, trajectory.csv, state.rds | population quantities, both decomposition terms, the pluralistic-ignorance verdict, convergence; all parameters; declarations per round; the full fixed point | always (state.rds optional) |

For cross-reference with the identifiability note: attitude and perceived
prevalence alone is its tier T0; degree adds T1; declaration T2; anonymous
contact perceptions T3; contacts' degrees T3'; the linked design T4.
