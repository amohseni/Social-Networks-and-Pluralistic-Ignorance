# Scenario smoke tests for the R engine, mirroring for-claude/models/pi-fp/smoke_tests.py.
# Run: Rscript tests/smoke_tests.R   (must print ALL PASS)
#   A. S1 produces both outcome classes from one generator; conformist types raise the PI basin probability.
#   B. S2-pure: misperception term exactly 0; PI from structure alone on heterogeneous graphs; 0 structure on regular graphs.
#   C. S3: tipping and persistence both occur; regular-graph persistence is pure misperception.
#   D. Decomposition identity to machine precision on every run.
#   E. Edge-end estimator: census q̂ - p̃ equals degree-weighted net falsification exactly.
#   F. Schema and extension checks (tiers, homophily rewiring preserves degrees, eigenvector option, internalization).

here <- tryCatch(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)))), error = function(e) ".")
root <- normalizePath(file.path(here, ".."))
source(file.path(root, "R", "engine.R")); source(file.path(root, "R", "measure.R")); source(file.path(root, "R", "export.R")); source(file.path(root, "R", "sweep.R"))

FAIL <- character(0)
check <- function(name, cond) { cat(if (isTRUE(cond)) "PASS " else "FAIL ", name, "\n", sep = ""); if (!isTRUE(cond)) FAIL <<- c(FAIL, name) }
gt_checked <- function(st) { gt <- ground_truth(st); stopifnot(abs(gt$decomposition_residual) < 1e-10); gt }
run <- function(...) run_scenario(list(...))

t0 <- Sys.time()
# A. S1
freqs <- list(); mis <- list(PI = c(), noPI = c())
for (lab in c("uniform", "beta13")) {
  cls <- character(0)
  for (s in 1:120) {
    p <- list(scenario = "S1", n = 100, topology = "er", mean_degree = 6, pi0 = 0.6, seed = s)
    if (lab == "beta13") { p$alpha_dist <- "beta"; p$alpha_a <- 1; p$alpha_b <- 3 }
    gt <- gt_checked(run_scenario(p))
    k <- if (gt$pi_signature) "PI" else "noPI"; cls <- c(cls, k)
    if (lab == "beta13") mis[[k]] <- c(mis[[k]], abs(gt$term_misperception))
  }
  freqs[[lab]] <- table(factor(cls, levels = c("noPI", "PI")))
  cat(sprintf("S1 outcome classes, alpha ~ %s, 120 seeds: noPI %d, PI %d\n", lab, freqs[[lab]]["noPI"], freqs[[lab]]["PI"]))
}
check("A1: both S1 outcome classes occur", all(sapply(freqs, function(f) f["noPI"] > 5)) && sum(sapply(freqs, function(f) f["PI"])) > 5)
check("A2: conformist-heavy types raise the PI basin probability", freqs$beta13["PI"] > freqs$uniform["PI"])
check("A3: PI runs carry larger absolute misperception than no-PI runs", mean(mis$PI) > mean(mis$noPI))

# B. S2-pure
gt <- gt_checked(run(scenario = "S2", n = 101, topology = "star", pi0 = 0.99, lambda = 50, pure = TRUE, seed = 1))
cat(sprintf("S2-pure star: p=%.4f mean_ghat=%.4f mis=%.4f struct=%.4f pi=%s\n", gt$p, gt$mean_ghat, gt$term_misperception, gt$term_structure, gt$pi_signature))
check("B1: star, zero misperception", abs(gt$term_misperception) < 1e-12)
check("B2: star, PI signature from structure alone", gt$pi_signature && gt$term_structure < 0)
hits <- 0
for (s in 1:30) { gt <- gt_checked(run(scenario = "S2", n = 300, topology = "ba", m = 2, pi0 = 0.7, lambda = 6, pure = TRUE, seed = s)); stopifnot(abs(gt$term_misperception) < 1e-12); hits <- hits + gt$pi_signature }
cat(sprintf("S2-pure BA(m=2), lambda=6: PI signature in %d/30 runs\n", hits))
check("B3: BA structure-only PI occurs in a majority of runs", hits >= 15)
gt <- gt_checked(run(scenario = "S2", n = 300, topology = "regular", k = 6, pi0 = 0.7, lambda = 6, pure = TRUE, seed = 3))
check("B4: regular graph forces structure term to 0 (Prop 2 corollary)", abs(gt$term_structure) < 1e-12 && !gt$pi_signature)

# C. S3
res <- t(sapply(1:60, function(s) { st <- run(scenario = "S3", n = 100, topology = "complete", psi = 0.7, seed = s); gt <- gt_checked(st); c(gt$pi_signature, mean(st$D)) }))
cat(sprintf("S3 complete, psi=0.7: %d/60 persist (PI), %d/60 tip\n", sum(res[, 1]), sum(res[, 2] < 0.5)))
check("C1: both persistence and tipping occur", sum(res[, 1]) > 3 && sum(res[, 2] < 0.5) > 3)
found <- NULL
for (s in 1:60) { st <- run(scenario = "S3", n = 200, topology = "regular", k = 8, psi = 0.7, alpha_dist = "beta", alpha_a = 1, alpha_b = 3, seed = 1000 + s); gt <- gt_checked(st); if (gt$pi_signature) { found <- gt; cat(sprintf("S3 regular persistence: seed %d, p=%.2f, mean_ghat=%.2f\n", 1000 + s, gt$p, gt$mean_ghat)); break } }
check("C2: an S3 persistence run exists on a regular graph", !is.null(found))
if (!is.null(found)) check("C3: its gap is pure misperception (structure term 0)", abs(found$term_structure) < 1e-12 && abs(found$term_misperception) > 0.1)

# E. edge-end estimator
st <- run(scenario = "S3", n = 400, topology = "regular", k = 8, psi = 0.65, seed = 7); gt <- gt_checked(st)
sv <- survey(st, 400, survey_seed = 8); est <- edge_end_estimates(sv$tiers$T3)
check("E1: census edge-end test equals degree-weighted net falsification", abs(est$net_misperception - gt$deg_weighted_net_falsification) < 1e-12)
sv2 <- survey(st, 200, survey_seed = 9); est2 <- edge_end_estimates(sv2$tiers$T3)
cat(sprintf("E: census net misperception %.4f, m=200 estimate %.4f\n", est$net_misperception, est2$net_misperception))
check("E2: m=n/2 estimate within 0.08 of census value", abs(est2$net_misperception - est$net_misperception) < 0.08)
st <- run(scenario = "S2", n = 301, topology = "ba", m = 2, pi0 = 0.7, lambda = 6, pure = TRUE, seed = 11)
est <- edge_end_estimates(survey(st, 301, 12)$tiers$T3)
check("E3: S2-pure census edge-end test is exactly zero", abs(est$net_misperception) < 1e-12)
bb <- edge_end_bootstrap(sv2$tiers$T3, B = 100)
check("E4: bootstrap CI brackets the point estimate", bb$ci_lower <= bb$estimate && bb$estimate <= bb$ci_upper)

# F. schema and extensions
st <- run(scenario = "S1", n = 80, topology = "ba", m = 2, seed = 5); sv <- survey(st, 40, 1)
check("F1: tier column schema", identical(names(sv$tiers$T0), c("ego_id", "a", "ghat")) && identical(names(sv$tiers$T2), c("ego_id", "a", "ghat", "d", "b")) &&
      identical(names(sv$tiers$T3), c("ego_id", "alter_slot", "a", "ghat", "d", "b", "ahat")) && "alter_d" %in% names(sv$tiers$T3p) && all(c("alter_id", "alter_a") %in% names(sv$tiers$T4)))
check("F2: T3 rows equal total sampled degree", nrow(sv$tiers$T3) == sum(sv$tiers$T1$d))
check("F3: T4 linkage is exact (alter_a equals the alter's attitude)", all(sv$tiers$T4$alter_a == st$a[sv$tiers$T4$alter_id]))
set.seed(1); g <- make_graph("er", 200, mean_degree = 6); a <- as.integer(runif(200) < 0.5)
g2 <- rewire_homophily(g, a, 0.5)
check("F4: homophily rewiring preserves degrees and raises same-attitude share",
      identical(g$deg, g2$deg) && mean(a[g2$edges[, 1]] == a[g2$edges[, 2]]) > mean(a[g$edges[, 1]] == a[g$edges[, 2]]))
gt <- gt_checked(run(scenario = "S2", n = 200, topology = "ba", m = 2, pi0 = 0.7, lambda = 4, pure = TRUE, centrality = "eigenvector", seed = 2))
check("F5: eigenvector-centrality S2 runs with zero misperception", abs(gt$term_misperception) < 1e-12)
st <- run(scenario = "S3", n = 100, topology = "complete", psi = 0.7, internalization = 0.5, seed = 3); gt <- gt_checked(st)
check("F6: internalization changes attitudes and still converges", gt$n_internalized > 0 && gt$converged && all(st$D == st$a))
for (tp in c("complete", "ring", "regular", "er", "ba", "star", "core_periphery", "small_world")) { g <- make_graph(tp, 50); stopifnot(all(g$deg >= 1), nrow(g$edges) > 0) }
check("F7: all topologies build with no isolates at n=50", TRUE)
td <- tempfile(); write_run(st, survey(st, 50, 1), td)
check("F8: write_run emits all files", all(file.exists(file.path(td, c("T0.csv", "T1.csv", "T2.csv", "T3.csv", "T3p.csv", "T4.csv", "ground_truth.json", "params.json", "state.rds", "trajectory.csv")))))
spec <- yaml::read_yaml(file.path(root, "grids", "smoke.yaml")); cells <- expand_grid_spec(spec)
td2 <- tempfile(); mf <- run_sweep(cells, td2, seeds_per_cell = 2, m_list = spec$m, name = "smoke")
check("F9: smoke sweep writes a manifest with one row per run", nrow(mf) == 2 * length(cells) && file.exists(file.path(td2, "manifest.csv")) && all(mf$converged))
spec1 <- yaml::read_yaml(file.path(root, "grids", "corpus-v1.yaml")); c1 <- expand_grid_spec(spec1)
cat(sprintf("corpus-v1 grid: %d cells; at %d seeds per cell = %d runs; rough time %.0f min\n", length(c1), spec1$seeds_per_cell, length(c1) * spec1$seeds_per_cell, estimate_runtime_seconds(c1, spec1$seeds_per_cell) / 60))
check("F10: corpus-v1 grid expands to the proposed cell count (669 plus core_periphery combinations)", length(c1) > 600)

cat(sprintf("\nelapsed %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(if (length(FAIL) == 0) "ALL PASS\n" else paste0("FAILURES: ", paste(FAIL, collapse = "; "), "\n"))
if (length(FAIL)) quit(status = 1)
