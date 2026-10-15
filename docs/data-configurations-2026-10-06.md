# Data configurations in the GUI (2026-10-06)

What changed, why, and the decisions Aydin can still reverse cheaply.

## What the problem statement asks for

The problem statement (Pluralistic Ignorance and the Friendship Paradox, "Five data configurations", board of 27 September) defines what a study records about each respondent: C1 (Y_i, P_i); C2 adds X_i; C3 adds d_i; C4 adds the ego network with the contacts' own (X_j, Y_j); C5 adds the global network A. The item P_Ni (perceived attitude of one's own contacts) can be added to any of them. The tool now writes every dataset in each configuration the user ticks.

## Mapping to the model

| Problem statement | Model | Column |
|---|---|---|
| Y_i private attitude | Y_i (code `st$Y`) | attitude |
| X_i public behavior | X_i, the declaration (code `st$X`) | declaration |
| P_i perceived population attitude | share of contacts the agent takes to hold 1, plus optional extrapolation noise | perceived_prevalence |
| P_Ni perceived attitude of own contacts | the same share, exact | perceived_contacts_mean |
| d_i | degree | degree |
| contacts' X_j, Y_j, d_j | X_j, Y_j, d_j | neighbor_declaration, neighbor_attitude, neighbor_degree |
| A | edge list | network_edges.csv |

The problem statement's Y is Likert; the model's is binary. The extrapolation term of its decomposition is zero in this model unless extrapolation noise is on, because respondents take the share among their contacts as their estimate for the population.

## Decisions taken (reversible)

1. **C5 is a census.** The problem statement says that at C5 "everything is known exactly", which needs every agent's answers, not only the sampled respondents' answers plus the edge list. So C5 = C4 for every agent + network_edges.csv. The sampled respondents keep identifiers 1..m in C5 too, so C1 to C4 are literally row subsets of C5.
2. **C4 contacts are identified.** The problem statement's C4 needs Y_j from the contacts, which requires the contact to be interviewed, so contacts carry identifiers shared across respondents (a contact named by several respondents has one identifier). The contacts' own answers (neighbor_attitude, neighbor_degree) are present only for contacts who answered; neighbor_declaration (public behavior) and perceived_neighbor_attitude (the respondent's belief) are always present. The share of non-sampled contacts who answer is a setting (default 1).
3. **C3a kept.** The anonymous per-contact perception design (the identifiability note's T3, where eta is estimable) is not one of the five; it sits between C3 and C4 and is kept as an optional sixth configuration, labeled as an addition.
4. **P_Ni is an option on every configuration**, off by default, as the problem statement describes it (one extra question). With noise off it equals perceived_prevalence, and the README says so.
5. **Identifiers and coding flips are shared across configurations** of a dataset, so the configurations can be handed out in stages (C1 first, then C3, then C4) and the analysts' estimates compared across stages on the same datasets.

## Download design

Two buttons on the Generate corpus tab: the analysts' package (one zip: an overview README plus one folder per configuration, each with its own codebook README and CSVs) and the key (one zip: key.csv, nodes.csv, edges.csv, corpus.json, README). A third control downloads a single configuration as a zip, for handing out one stage. The Explore tab's run download uses the same layout for one population as a census, with the ground truth in `key/`. The earlier six separate file buttons are gone.

## Code

`R/measure.R`: CONFIGS, config_summary, survey (now records every agent and marks the sample; new columns perceived_contacts_mean, neighbor_declaration, neighbor_responded; new arguments noise_sd, alter_response_rate), survey_sample, apply_config; apply_design kept for sweep.R. `R/corpus.R`: settings configs, add_pn, noise_sd, alter_response_rate; write_config_files, config_codebook, config_readme, overview_readme, key_readme; output layout for_analysts/<C>/ and key/. `R/export.R`: write_run in the same layout; make_zip. `app.R`: configuration controls, the two downloads, Guide section 7 generated from CONFIGS. `tests/smoke_tests.R`: F1, F3, F4, F8, H2, H3, H3a, H3c updated; I1 to I5 added. All pass (12 s).

## Notation alignment (2026-10-07)

Aydin's instruction: align the app's notation with the problem statement's. Done throughout the GUI text, the Guide (new section 0, Notation), the control labels, the key, the READMEs, the code comments, and the state fields: A_i is now Y_i (`st$Y`), D_i is X_i (`st$X`), N_i(D) is S_i(x), the neighbor set is N_i, hat A_ij is P_ij, the population size is N in the GUI and the key (`n` remains the code parameter), the respondent count is n. Ground-truth and key fields: `mu_Y`, `mu_X`, `mu_P`, `Delta_G`, `exposure_bias` (was structure term), `perception_error` (was misperception term), `perception_error_gross`, `Delta_F` (new), `mu_Y_edge`, `mu_PN_edge`, `eta` (was the edge-end statistic; it is the statement's degree-weighted perception error exactly), `eta_sample_as_coded`. Two symbols are additions and are labeled as such: P_ij and c_i.
