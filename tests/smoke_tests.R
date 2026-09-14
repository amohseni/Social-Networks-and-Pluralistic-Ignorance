# Acceptance tests for the R engine (model of 2026-09-11).
# Run: Rscript tests/smoke_tests.R   (must print ALL PASS)
#   A. Random start: both outcome classes occur; conformist-heavy types raise the PI frequency.
#   B. Well-connected minority without conformity: misperception exactly 0; PI from structure alone
#      on heterogeneous networks; structure term 0 on regular networks; all centrality measures run.
#   C. Private change of mind: tipping under uniform conformity, persistence under beta(3,1);
#      regular-network persistence is pure misperception.
#   D. Decomposition identity to machine precision on every run.
#   E. Edge-end estimator: census value equals degree-weighted net falsification exactly.
#   F. Survey designs, exports, sweep pipeline, extensions.
#   G. Best-response sanity.
#   H. Blinded corpus: every dataset shows the pattern, coding flips are consistent, ids anonymous, scoring works.

here <- tryCatch(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)))), error = function(e) ".")
root <- normalizePath(file.path(here, ".."))
source(file.path(root, "R", "engine.R")); source(file.path(root, "R", "measure.R")); source(file.path(root, "R", "export.R")); source(file.path(root, "R", "sweep.R")); source(file.path(root, "R", "corpus.R"))

FAIL <- character(0)
check <- function(name, cond) { cat(if (isTRUE(cond)) "PASS " else "FAIL ", name, "\n", sep = ""); if (!isTRUE(cond)) FAIL <<- c(FAIL, name) }
gt_checked <- function(st) { gt <- ground_truth(st); stopifnot(abs(gt$decomposition_residual) < 1e-10); gt }
run <- function(...) run_scenario(list(...))
t0 <- Sys.time()

# G. best response
nb <- list(c(2L, 3L, 4L), 1L, 1L, 1L)
check("G1: c = 0 declares its attitude whatever the neighbors say", best_response(1L, a = c(1L, 0L, 0L, 0L), cpar = c(0, 0, 0, 0), D = c(1L, 0L, 0L, 0L), nb) == 1L)
check("G2: c = 1 follows the local majority", best_response(1L, a = c(1L, 0L, 0L, 0L), cpar = c(1, 1, 1, 1), D = c(1L, 0L, 0L, 0L), nb) == 0L)
check("G3: the switch point is (1 - c) = c (1 - 2 N): c = 0.5 with no support ties toward the attitude, c = 0.51 flips",
      best_response(1L, c(1L, 0L, 0L, 0L), c(0.5, 0, 0, 0), c(1L, 0L, 0L, 0L), nb) == 1L && best_response(1L, c(1L, 0L, 0L, 0L), c(0.51, 0, 0, 0), c(1L, 0L, 0L, 0L), nb) == 0L)

# A. random start (preferential attachment, n = 200, pi0 = 0.6)
freqs <- list(); mis <- list(PI = c(), noPI = c())
for (lab in c("uniform", "beta61")) {
  cls <- character(0)
  for (s in 1:60) {
    p <- list(scenario = "S1", n = 200, topology = "ba", m = 2, pi0 = 0.6, seed = s)
    if (lab == "beta61") { p$c_dist <- "beta"; p$c_a <- 6; p$c_b <- 1 }
    gt <- gt_checked(run_scenario(p))
    k <- if (gt$pluralistic_ignorance) "PI" else "noPI"; cls <- c(cls, k)
    if (lab == "beta61") mis[[k]] <- c(mis[[k]], abs(gt$term_misperception))
  }
  freqs[[lab]] <- table(factor(cls, levels = c("noPI", "PI")))
  cat(sprintf("Random start, c ~ %s, 60 seeds: no PI %d, PI %d\n", lab, freqs[[lab]]["noPI"], freqs[[lab]]["PI"]))
}
check("A1: both outcome classes occur", freqs$beta61["noPI"] > 5 && freqs$beta61["PI"] > 3)
check("A2: conformist-heavy types raise the PI frequency", freqs$beta61["PI"] > freqs$uniform["PI"])
check("A3: PI runs carry larger absolute misperception than no-PI runs", mean(mis$PI) > mean(mis$noPI))

# B. well-connected minority, no conformity
gt <- gt_checked(run(scenario = "S2", n = 101, topology = "star", pi0 = 0.99, lambda = 50, pure = TRUE, seed = 1))
cat(sprintf("Star: p=%.4f majority perceived share=%.4f mis=%.4f struct=%.4f PI=%s\n", gt$p, gt$majority_mean_perceived_share, gt$term_misperception, gt$term_structure, gt$pluralistic_ignorance))
check("B1: star, zero misperception", abs(gt$term_misperception) < 1e-12)
check("B2: star, pluralistic ignorance from structure alone", gt$pluralistic_ignorance && gt$term_structure < 0)
hits <- 0
for (s in 1:30) { gt <- gt_checked(run(scenario = "S2", n = 300, topology = "ba", m = 2, pi0 = 0.7, lambda = 6, pure = TRUE, seed = s)); stopifnot(abs(gt$term_misperception) < 1e-12); hits <- hits + gt$pluralistic_ignorance }
cat(sprintf("Preferential attachment (m=2), lambda=6, no conformity: PI in %d/30 runs\n", hits))
check("B3: structure-only PI occurs in a majority of preferential-attachment runs", hits >= 15)
gt <- gt_checked(run(scenario = "S2", n = 300, topology = "regular", k = 6, pi0 = 0.7, lambda = 6, pure = TRUE, seed = 3))
check("B4: regular network forces the structure term to 0 and no PI", abs(gt$term_structure) < 1e-12 && !gt$pluralistic_ignorance)
for (cen in c("eigenvector", "betweenness")) { gt <- gt_checked(run(scenario = "S2", n = 200, topology = "ba", m = 2, pi0 = 0.7, lambda = 4, pure = TRUE, centrality = cen, seed = 2)); check(sprintf("B5: %s centrality runs with zero misperception", cen), abs(gt$term_misperception) < 1e-12 && gt$converged) }
gt0 <- gt_checked(run(scenario = "S2", n = 300, topology = "ba", m = 2, pi0 = 0.7, lambda = 0, pure = TRUE, seed = 4))
check("B6: lambda = 0 places the minority at random (|structure term| small)", abs(gt0$term_structure) < 0.05)

# C. private change of mind
out <- list()
for (lab in c("uniform", "beta31")) {
  res <- t(sapply(1:40, function(s) { p <- list(scenario = "S3", n = 100, topology = "complete", psi = 0.7, seed = s); if (lab == "beta31") { p$c_dist <- "beta"; p$c_a <- 3; p$c_b <- 1 }
    st <- run_scenario(p); gt <- gt_checked(st); c(gt$pluralistic_ignorance, mean(st$D)) }))
  out[[lab]] <- c(persist = sum(res[, 1]), tip = sum(res[, 2] < 0.5))
  cat(sprintf("Private change (complete, psi=0.7), c ~ %s: %d/40 persist (PI), %d/40 tip\n", lab, out[[lab]]["persist"], out[[lab]]["tip"]))
}
check("C1: uniform conformity mostly tips; beta(3,1) mostly persists", out$uniform["tip"] > 30 && out$beta31["persist"] > 30)
found <- NULL
for (s in 1:40) { st <- run(scenario = "S3", n = 200, topology = "regular", k = 8, psi = 0.7, c_dist = "beta", c_a = 3, c_b = 1, seed = 1000 + s); gt <- gt_checked(st); if (gt$pluralistic_ignorance) { found <- gt; break } }
check("C2: a persistence run exists on a regular network", !is.null(found))
if (!is.null(found)) check("C3: its gap is pure misperception (structure term 0)", abs(found$term_structure) < 1e-12 && abs(found$term_misperception) > 0.1)

# E. edge-end estimator
st <- run(scenario = "S3", n = 400, topology = "regular", k = 8, psi = 0.65, c_dist = "beta", c_a = 2, c_b = 1, seed = 7); gt <- gt_checked(st)
sv <- survey(st, 400, survey_seed = 8); est <- edge_end_estimates(sv$respondents, sv$ego_network)
check("E1: census edge-end test equals degree-weighted net falsification", abs(est$net_misperception - gt$deg_weighted_net_falsification) < 1e-12)
sv2 <- survey(st, 200, survey_seed = 9); est2 <- edge_end_estimates(sv2$respondents, sv2$ego_network)
cat(sprintf("Edge-end: census %.4f, m=200 estimate %.4f\n", est$net_misperception, est2$net_misperception))
check("E2: m = n/2 estimate within 0.08 of the census value", abs(est2$net_misperception - est$net_misperception) < 0.08)
st <- run(scenario = "S2", n = 301, topology = "ba", m = 2, pi0 = 0.7, lambda = 6, pure = TRUE, seed = 11); sv <- survey(st, 301, 12)
check("E3: structure-only census edge-end test is exactly zero", abs(edge_end_estimates(sv$respondents, sv$ego_network)$net_misperception) < 1e-12)
bb <- edge_end_bootstrap(sv2$respondents, sv2$ego_network, B = 100)
check("E4: bootstrap CI brackets the point estimate", bb$ci_lower <= bb$estimate && bb$estimate <= bb$ci_upper && abs(bb$estimate - est2$net_misperception) < 1e-12)

# F. survey designs, exports, sweep, extensions
st <- run(scenario = "S1", n = 80, topology = "ba", m = 2, seed = 5); sv <- survey(st, 40, 1)
check("F1: respondents and ego_network columns", identical(names(sv$respondents), c("respondent_id", "attitude", "perceived_prevalence", "degree", "declaration")) &&
      identical(names(sv$ego_network), c("respondent_id", "neighbor_slot", "perceived_neighbor_attitude", "neighbor_degree", "neighbor_id", "neighbor_attitude")))
ds <- apply_design(sv, c("degree", "ego_perceptions"))
check("F2: a survey design keeps only the recorded columns", identical(names(ds$respondents), c("respondent_id", "attitude", "perceived_prevalence", "degree")) &&
      identical(names(ds$ego_network), c("respondent_id", "neighbor_slot", "perceived_neighbor_attitude")) && is.null(apply_design(sv, "degree")$ego_network))
check("F3: ego_network rows equal total sampled degree", nrow(sv$ego_network) == sum(sv$respondents$degree))
check("F4: linked design is exact", all(sv$ego_network$neighbor_attitude == st$a[sv$ego_network$neighbor_id]) && all(sv$ego_network$perceived_neighbor_attitude == st$D[sv$ego_network$neighbor_id]))
set.seed(1); g <- make_graph("ba", 200, m = 2); a <- as.integer(runif(200) < 0.5); g2 <- rewire_homophily(g, a, 0.5)
check("F5: homophily rewiring preserves degrees and raises same-attitude share", identical(g$deg, g2$deg) && mean(a[g2$edges[, 1]] == a[g2$edges[, 2]]) > mean(a[g$edges[, 1]] == a[g$edges[, 2]]))
st <- run(scenario = "S3", n = 100, topology = "complete", psi = 0.7, c_dist = "beta", c_a = 3, c_b = 1, internalization = 0.5, seed = 3); gt <- gt_checked(st)
check("F6: internalization changes attitudes and converges with no falsifier left", gt$n_internalized > 0 && gt$converged && all(st$D == st$a))
for (tp in c("complete", "ring", "regular", "er", "ba", "star", "core_periphery", "small_world")) { g <- make_graph(tp, 50); stopifnot(all(g$deg >= 1), nrow(g$edges) > 0) }
check("F7: all topologies build with no isolates at n = 50", TRUE)
td <- tempfile(); write_run(st, survey(st, 50, 1), td)
check("F8: write_run emits all files", all(file.exists(file.path(td, c("respondents.csv", "ego_network.csv", "ground_truth.json", "params.json", "state.rds", "trajectory.csv")))))
td1 <- tempfile(); write_run(st, survey(st, 50, 1), td1, include = "degree")
check("F9: a design without contact perceptions writes no ego_network.csv", file.exists(file.path(td1, "respondents.csv")) && !file.exists(file.path(td1, "ego_network.csv")))
spec <- yaml::read_yaml(file.path(root, "grids", "smoke.yaml")); cells <- expand_grid_spec(spec)
td2 <- tempfile(); mf <- run_sweep(cells, td2, seeds_per_cell = 2, m_list = spec$m, name = "smoke")
check("F10: smoke sweep writes a manifest with one row per run", nrow(mf) == 2 * length(cells) && file.exists(file.path(td2, "manifest.csv")) && all(mf$converged) && "pluralistic_ignorance" %in% names(mf))
spec1 <- yaml::read_yaml(file.path(root, "grids", "corpus-v1.yaml")); c1 <- expand_grid_spec(spec1)
cat(sprintf("corpus-v1 grid: %d cells; at %d seeds per cell = %d runs; rough time %.0f min\n", length(c1), spec1$seeds_per_cell, length(c1) * spec1$seeds_per_cell, estimate_runtime_seconds(c1, spec1$seeds_per_cell, 4) / 60))
check("F11: corpus-v1 grid expands", length(c1) > 600)

# H. blinded corpus
s <- default_corpus_settings(); s$per_scenario[] <- 3L; s$n_range <- c(120L, 200L); s$respondents <- 80L
td3 <- tempfile(); key <- generate_blinded_corpus(s, td3)
check("H1: corpus has the requested datasets, all showing the pattern in the population", nrow(key) == 9 && all(table(key$scenario_code) == 3) && all(key$majority_mean_perceived_share < 0.5))
check("H2: blinded folder has README and one folder per dataset; key kept outside it",
      file.exists(file.path(td3, "blinded", "README.txt")) && length(list.dirs(file.path(td3, "blinded", "datasets"), recursive = FALSE)) == 9 && file.exists(file.path(td3, "key.csv")) && !file.exists(file.path(td3, "blinded", "key.csv")))
r1 <- read.csv(file.path(td3, "blinded", "datasets", key$dataset_id[1], "respondents.csv")); e1 <- read.csv(file.path(td3, "blinded", "datasets", key$dataset_id[1], "ego_network.csv"))
check("H3: blinded files carry only the recorded columns", identical(names(r1), c("respondent_id", "attitude", "perceived_prevalence", "degree", "declaration")) && identical(names(e1), c("respondent_id", "neighbor_slot", "perceived_neighbor_attitude", "neighbor_degree")))
det <- readRDS(file.path(td3, "key_details", paste0(key$dataset_id[1], ".rds")))
maj_coded <- if (mean(r1$attitude) >= 0.5) 1L else 0L
check("H4: the majority as coded matches the key and the sample shows the pattern the same way",
      maj_coded == key$majority_attitude_as_coded[1] && key$pattern_in_sample[1] == sample_shows_pattern(r1))
check("H5: consistency after coding flip: perceived prevalence equals the mean perceived contact attitude",
      max(abs(r1$perceived_prevalence - tapply(e1$perceived_neighbor_attitude, factor(e1$respondent_id, levels = r1$respondent_id), mean))) < 1e-12)
check("H6: mechanism labels follow the scenarios", all(key$mechanism[key$scenario_code == "S2-pure"] == "friendship paradox") && all(key$mechanism[key$scenario_code != "S2-pure"] == "genuine pluralistic ignorance"))
pred <- data.frame(dataset_id = key$dataset_id, prediction = ifelse(key$falsified_share > 0, "genuine", "structure"), score = key$falsified_share)
sc <- score_predictions(key, pred)
check("H7: scoring a perfect prediction gives accuracy 1 and AUC 1", sc$accuracy == 1 && sc$auc == 1 && sc$n_scored == 9)
pred2 <- pred; pred2$prediction <- rev(pred2$prediction)
check("H8: scoring reports imperfect predictions", score_predictions(key, pred2)$accuracy < 1)

cat(sprintf("\nelapsed %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(if (length(FAIL) == 0) "ALL PASS\n" else paste0("FAILURES: ", paste(FAIL, collapse = "; "), "\n"))
if (length(FAIL)) quit(status = 1)
