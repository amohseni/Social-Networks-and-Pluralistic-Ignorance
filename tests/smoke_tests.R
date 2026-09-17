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
#   H9. The two conformity versions of the well-connected minority generate and are keyed correctly.

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

# A. random start (preferential attachment, n = 150, pi0 = 0.6)
res <- list()
for (lab in c("uniform", "beta121", "sincere")) {
  res[[lab]] <- t(sapply(1:80, function(s) {
    p <- list(scenario = "S1", n = 150, topology = "ba", m = 2, pi0 = 0.6, seed = s)
    if (lab == "beta121") { p$c_dist <- "beta"; p$c_a <- 12; p$c_b <- 1 }
    if (lab == "sincere") { p$c_dist <- "point"; p$c_point <- 0 }
    gt <- gt_checked(run_scenario(p)); c(pattern = gt$pattern_present, prevalence = gt$pi_prevalence, apparent = gt$apparent_share) }))
  cat(sprintf("Random start, c ~ %s, 80 seeds: pattern in %d runs; mean prevalence %.3f; mean share believing local minority %.3f\n", lab, sum(res[[lab]][, "pattern"]), mean(res[[lab]][, "prevalence"]), mean(res[[lab]][, "apparent"])))
}
check("A1: the pattern occurs under strong conformity and never with sincere declarations", sum(res$beta121[, "pattern"]) >= 2 && sum(res$sincere[, "pattern"]) == 0)
check("A2: conformist-heavy types raise the prevalence of pluralistic ignorance", mean(res$beta121[, "prevalence"]) > mean(res$uniform[, "prevalence"]) && all(res$sincere[, "prevalence"] == 0))
check("A3: runs showing the pattern have higher prevalence than runs without", mean(res$beta121[res$beta121[, "pattern"] == 1, "prevalence"]) > mean(res$beta121[res$beta121[, "pattern"] == 0, "prevalence"]))
gt <- gt_checked(run(scenario = "S1", n = 150, topology = "ba", m = 2, pi0 = 0.6, c_dist = "beta", c_a = 12, c_b = 1, seed = which(res$beta121[, "pattern"] == 1)[1]))
check("A4: a random-start run with the pattern has positive prevalence, for holders of both attitudes", gt$pi_prevalence > 0 && gt$pi_prevalence_a1 > 0 && gt$pi_prevalence_a0 > 0)

# B. well-connected minority, no conformity
gt <- gt_checked(run(scenario = "S2", n = 101, topology = "star", pi0 = 0.99, lambda = 50, pure = TRUE, seed = 1))
cat(sprintf("Star: p=%.4f share believing local minority=%.4f mis=%.4f struct=%.4f pattern=%s PI prevalence=%.3f\n", gt$p, gt$apparent_share, gt$term_misperception, gt$term_structure, gt$pattern_present, gt$pi_prevalence))
check("B1: star, zero misperception", abs(gt$term_misperception) < 1e-12)
check("B2: star, the pattern with zero prevalence of pluralistic ignorance", gt$pattern_present && gt$term_structure < 0 && gt$pi_prevalence == 0)
hits <- 0
for (s in 1:30) { gt <- gt_checked(run(scenario = "S2", n = 300, topology = "ba", m = 2, pi0 = 0.7, lambda = 6, pure = TRUE, seed = s)); stopifnot(abs(gt$term_misperception) < 1e-12, gt$pi_prevalence == 0); hits <- hits + gt$pattern_present }
cat(sprintf("Preferential attachment (m=2), lambda=6, no conformity: PI in %d/30 runs\n", hits))
check("B3: the pattern with conformity off occurs in a majority of preferential-attachment runs, always with zero prevalence", hits >= 15)
gt <- gt_checked(run(scenario = "S2", n = 300, topology = "regular", k = 6, pi0 = 0.7, lambda = 6, pure = TRUE, seed = 3))
check("B4: regular network forces the structure term to 0 and no pattern", abs(gt$term_structure) < 1e-12 && !gt$pattern_present)
for (cen in c("eigenvector", "betweenness")) { gt <- gt_checked(run(scenario = "S2", n = 200, topology = "ba", m = 2, pi0 = 0.7, lambda = 4, pure = TRUE, centrality = cen, seed = 2)); check(sprintf("B5: %s centrality runs with zero misperception", cen), abs(gt$term_misperception) < 1e-12 && gt$converged) }
gt0 <- gt_checked(run(scenario = "S2", n = 300, topology = "ba", m = 2, pi0 = 0.7, lambda = 0, pure = TRUE, seed = 4))
check("B6: lambda = 0 places the minority at random (|structure term| small)", abs(gt0$term_structure) < 0.05)

# C. private change of mind
out <- list()
for (lab in c("uniform", "beta31")) {
  res <- t(sapply(1:40, function(s) { p <- list(scenario = "S3", n = 100, topology = "complete", psi = 0.7, seed = s); if (lab == "beta31") { p$c_dist <- "beta"; p$c_a <- 3; p$c_b <- 1 }
    st <- run_scenario(p); gt <- gt_checked(st); c(gt$pattern_present && gt$pi_prevalence > 0, mean(st$D)) }))
  out[[lab]] <- c(persist = sum(res[, 1]), tip = sum(res[, 2] < 0.5))
  cat(sprintf("Private change (complete, psi=0.7), c ~ %s: %d/40 persist (PI), %d/40 tip\n", lab, out[[lab]]["persist"], out[[lab]]["tip"]))
}
check("C1: uniform conformity mostly tips; beta(3,1) mostly persists", out$uniform["tip"] > 30 && out$beta31["persist"] > 30)
found <- NULL
for (s in 1:40) { st <- run(scenario = "S3", n = 200, topology = "regular", k = 8, psi = 0.7, c_dist = "beta", c_a = 3, c_b = 1, seed = 1000 + s); gt <- gt_checked(st); if (gt$pattern_present && gt$pi_prevalence > 0) { found <- gt; break } }
check("C2: a persistence run exists on a regular network", !is.null(found))
if (!is.null(found)) check("C3: its gap is pure misperception (structure term 0)", abs(found$term_structure) < 1e-12 && abs(found$term_misperception) > 0.1)

# B7. with conformity on, gross misperception is large even where the net term is small
gt <- gt_checked(run(scenario = "S2", n = 150, topology = "ba", m = 2, pi0 = 0.7, lambda = 8, pure = FALSE, c_dist = "beta", c_a = 3, c_b = 1, seed = 6081))
cat(sprintf("Well-connected minority with conformity: net misperception %.3f, gross %.3f, falsified share %.3f\n", gt$term_misperception, gt$misperception_gross, gt$falsified_share))
check("B7: gross misperception exceeds the absolute net term with conformity on", gt$misperception_gross > abs(gt$term_misperception) && gt$misperception_gross > 0.1)
gt0 <- gt_checked(run(scenario = "S2", n = 150, topology = "ba", m = 2, pi0 = 0.7, lambda = 8, pure = TRUE, seed = 6081))
check("B8: gross misperception is exactly zero with conformity off", gt0$misperception_gross == 0)
gtc <- gt_checked(run(scenario = "S2", n = 150, topology = "ba", m = 2, pi0 = 0.7, lambda = 8, pure = FALSE, perception = "attitudes", c_dist = "beta", c_a = 3, c_b = 1, seed = 6081))
cat(sprintf("Conformity to true attitudes: falsified share %.3f, misperception term %.4f, gross %.4f, PI prevalence %.3f, pattern %s\n", gtc$falsified_share, gtc$term_misperception, gtc$misperception_gross, gtc$pi_prevalence, gtc$pattern_present))
check("B9: conformity to true attitudes falsifies but never misperceives, so prevalence is zero", gtc$falsified_share > 0 && gtc$misperception_gross == 0 && abs(gtc$term_misperception) < 1e-12 && gtc$pi_prevalence == 0)
svc <- survey(run(scenario = "S2", n = 150, topology = "ba", m = 2, pi0 = 0.7, lambda = 8, pure = FALSE, perception = "attitudes", c_dist = "beta", c_a = 3, c_b = 1, seed = 6081), 150, 1)
check("B10: clairvoyant survey reports true contact attitudes and a zero edge-end test", all(svc$ego_network$perceived_neighbor_attitude == svc$ego_network$neighbor_attitude) && abs(edge_end_estimates(svc$respondents, svc$ego_network)$net_misperception) < 1e-12)

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
check("F10: smoke sweep writes a manifest with one row per run", nrow(mf) == 2 * length(cells) && file.exists(file.path(td2, "manifest.csv")) && all(mf$converged) && all(c("pi_prevalence", "pattern_present") %in% names(mf)))
spec1 <- yaml::read_yaml(file.path(root, "grids", "corpus-v1.yaml")); c1 <- expand_grid_spec(spec1)
cat(sprintf("corpus-v1 grid: %d cells; at %d seeds per cell = %d runs; rough time %.0f min\n", length(c1), spec1$seeds_per_cell, length(c1) * spec1$seeds_per_cell, estimate_runtime_seconds(c1, spec1$seeds_per_cell, 4) / 60))
check("F11: corpus-v1 grid expands", length(c1) > 600)

# H. blinded corpus
s <- default_corpus_settings(); s$per_scenario[] <- 0L; s$per_scenario[c("S1", "S3", "S2-pure")] <- 3L; s$n_range <- c(120L, 200L); s$respondents <- 80L; s$conformity_a_range <- c(8, 12)
td3 <- tempfile(); key <- generate_blinded_corpus(s, td3)
check("H1: corpus has the requested datasets, all showing the pattern in the population", nrow(key) == 9 && all(table(key$scenario_code) == 3) && all(key$apparent_share > 0.5))
rall <- read.csv(file.path(td3, "blinded", "respondents.csv")); eall <- read.csv(file.path(td3, "blinded", "ego_network.csv"))
check("H2: blinded folder has README and stacked CSVs covering every dataset; key kept outside it",
      file.exists(file.path(td3, "blinded", "README.txt")) && length(unique(rall$dataset_id)) == 9 && file.exists(file.path(td3, "key.csv")) && !file.exists(file.path(td3, "blinded", "key.csv")))
r1 <- rall[rall$dataset_id == key$dataset_id[1], -1]; e1 <- eall[eall$dataset_id == key$dataset_id[1], -1]
check("H3: blinded files carry only the recorded columns", identical(names(rall), c("dataset_id", "respondent_id", "attitude", "perceived_prevalence", "degree", "declaration")) && identical(names(eall), c("dataset_id", "respondent_id", "neighbor_slot", "perceived_neighbor_attitude", "neighbor_degree")))
nodes <- read.csv(file.path(td3, "key_details", "nodes.csv")); edges <- read.csv(file.path(td3, "key_details", "edges.csv"))
mm <- merge(rall, nodes, by.x = c("dataset_id", "respondent_id"), by.y = c("dataset_id", "node_id"))
check("H3b: nodes.csv agrees with respondents.csv on every shared column and prevalence", nrow(mm) == nrow(rall) && all(mm$attitude.x == mm$attitude.y) && all(mm$declaration.x == mm$declaration.y) && all(mm$degree.x == mm$degree.y) && all(mm$sampled) &&
      max(abs(tapply(nodes$pluralistic_ignorance, nodes$dataset_id, mean)[key$dataset_id] - key$pi_prevalence)) < 1e-12 && all(edges$node_a != edges$node_b))
maj_coded <- if (mean(r1$attitude) >= 0.5) 1L else 0L
check("H4: the majority as coded matches the key and the sample shows the pattern the same way",
      maj_coded == key$majority_attitude_as_coded[1] && key$pattern_in_sample[1] == sample_shows_pattern(r1) && key$pattern_in_sample[1])
check("H5: consistency after coding flip: perceived prevalence equals the mean perceived contact attitude",
      max(abs(r1$perceived_prevalence - tapply(e1$perceived_neighbor_attitude, factor(e1$respondent_id, levels = r1$respondent_id), mean))) < 1e-12)
check("H6: mechanism labels follow the scenarios and prevalence is zero exactly for conformity off", all(key$mechanism[key$scenario_code == "S2-pure"] == "friendship paradox") && all(key$mechanism[key$scenario_code != "S2-pure"] == "genuine pluralistic ignorance") && all(key$pi_prevalence[key$scenario_code == "S2-pure"] == 0) && all(key$pi_prevalence[key$scenario_code != "S2-pure"] > 0))
pred <- data.frame(dataset_id = key$dataset_id, prediction = ifelse(key$falsified_share > 0, "genuine", "structure"), score = key$falsified_share, pi_prevalence = key$pi_prevalence)
sc <- score_predictions(key, pred)
check("H7: scoring a perfect prediction gives accuracy 1, AUC 1, and zero prevalence error", sc$accuracy == 1 && sc$auc == 1 && sc$n_scored == 9 && sc$prevalence_mae == 0)
pred2 <- pred; pred2$prediction <- rev(pred2$prediction)
check("H8: scoring reports imperfect predictions", score_predictions(key, pred2)$accuracy < 1)
s2 <- s; s2$per_scenario[] <- 0L; s2$per_scenario[c("S2-mixed", "S2-clairvoyant")] <- 2L
key2 <- generate_blinded_corpus(s2, tempfile())
s3 <- s; s3$per_scenario[] <- 0L; s3$per_scenario["S3"] <- 2L; s3$network <- "small_world"
key3 <- generate_blinded_corpus(s3, tempfile())
check("H10: another network family runs through the corpus and is named in the key", nrow(key3) == 2 && all(key3$network == "small world (Watts-Strogatz)") && all(key3$ring_contacts %in% c(4, 6, 8)) && all(key3$rewiring_share >= 0.05 & key3$rewiring_share <= 0.3))
check("H9: conformity versions of the well-connected minority are generated and keyed", nrow(key2) == 4 && all(key2$mechanism == "friendship paradox") && all(key2$perception[key2$scenario_code == "S2-clairvoyant"] == "attitudes") && all(key2$pi_prevalence[key2$scenario_code == "S2-clairvoyant"] == 0) && all(key2$falsified_share > 0))

cat(sprintf("\nelapsed %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(if (length(FAIL) == 0) "ALL PASS\n" else paste0("FAILURES: ", paste(FAIL, collapse = "; "), "\n"))
if (length(FAIL)) quit(status = 1)
