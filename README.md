# Social networks and pluralistic ignorance: PI/FP data-generation lab

R implementation of the generative models for the pluralistic-ignorance vs
friendship-paradox identifiability project, with a Shiny GUI for generating
labeled survey data from each mechanism and their mixtures.

Model, in the problem statement's notation: N agents on an undirected network
A; agent i has private attitude Y_i, public declaration X_i (the statement's
outward behavior) and conformity parameter c_i; declaring x pays
c_i S_i(x) + (1 - c_i) 1[x = Y_i], with S_i(x) the share of i's contacts that
i takes to hold x (P_ij = X_j); asynchronous best response in random order
until a quiet round. Pluralistic ignorance is a property of an individual agent: it holds the
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
- `R/measure.R`: ground truth (exposure bias and perception error, Delta_F, eta, pluralistic-ignorance verdict), survey extraction, the data configurations C1 to C5, eta with bootstrap.
- `R/export.R`: per-run export in the same layout as a corpus (`for_analysts/<C>/`, `key/`), and the zip helper.
- `R/sweep.R`: research sweeps over parameter grids with `manifest.csv` (CLI only).
- `R/corpus.R`: the generated corpus (datasets that all show the pattern, from every scenario, written per data configuration, with a key kept aside) and the scorer for analysts' predictions; also a CLI.
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
`<out>/for_analysts/` and `<out>/key/`. Pluralistic ignorance obtains in
every dataset, in the population and in the sample. The process is genuine
pluralistic ignorance (random start, private change of mind) or the friendship
paradox (well-connected minority, in three versions). Parameters are drawn at
random for each dataset; the attitude coding is reversed in a random half of
the datasets; respondent identifiers and dataset numbers are assigned at
random. `score_predictions()` compares a predictions CSV (dataset_id,
pi_prevalence, prediction in {genuine, structure}, optional score) with the key.

## Data configurations

A data configuration is what a study records about each respondent. The five
configurations of the problem statement (board of 27 September), plus C3a
between C3 and C4, are the export interface; every dataset is written once per
ticked configuration, in its own folder under `for_analysts/`, with the same
`dataset_id` and `respondent_id` throughout, so the configurations can be
handed to analysts one at a time.

| Code | Records | Files |
|---|---|---|
| C1 | attitude Y_i, perceived prevalence P_i | respondents.csv |
| C2 | C1 + own declaration X_i | respondents.csv |
| C3 | C2 + number of contacts d_i | respondents.csv |
| C3a | C3 + perceived attitude of each contact, contacts anonymous | + ego_network.csv |
| C4 | C3 + ego network: contacts identified, with their own answers (X_j, Y_j, d_j) when they answered | + ego_network.csv |
| C5 | C4 for every agent (a census) + the global network A | + ego_network.csv, network_edges.csv |

Options, in every configuration: the item P_Ni (`perceived_contacts_mean`,
the respondent's estimate of the share of their own contacts holding 1; one
extra survey question), extrapolation noise on `perceived_prevalence`, and the
share of named contacts who answer the survey (C4; non-answering contacts have
empty own-answer columns). Each configuration folder carries a README.txt
codebook; `for_analysts/README.txt` explains the nesting.

The key folder holds `key.csv` (one row per dataset: process, scenario
version, every parameter, the prevalence of pluralistic ignorance overall and
by attitude as coded, the exposure bias and the perception error, Delta_F,
eta on the population and on the sample), `nodes.csv` and `edges.csv` (the full final state of every
population, numbered as in the analysts' files), `corpus.json` (the settings)
and a README.

The GUI offers two downloads, the analysts' package and the key, each as one
zip, plus a single-configuration zip. The Explore tab's run download is the
same layout for one population as a census.

## Notation

The code, the GUI text, the key and the READMEs use the problem statement's
symbols: Y private attitude (`attitude`), X public declaration (`declaration`),
P perceived population share (`perceived_prevalence`), P_N perceived share among
own contacts (`perceived_contacts_mean`), d degree (`degree`), N population
size, n respondents, A the adjacency matrix, mu_Y / mu_X / mu_P population
means, Delta_G = mu_P - mu_Y, Delta_F the exposure distortion of behavior, and
eta the degree-weighted perception error. Two additions: P_ij, the attitude i
takes contact j to hold (`perceived_neighbor_attitude`), and c_i, the conformity
parameter. In the R code the state fields are `st$Y` and `st$X`; the population
size parameter is `n` in code and `N` in the GUI and the key.
