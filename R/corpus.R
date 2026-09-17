# Generated corpus: datasets in which pluralistic ignorance obtains, produced by
# either process, delivered as files for the analysts plus a separate key.
#
#   generate_blinded_corpus(settings, out_dir, progress)  ->  key data frame
#   score_predictions(key, predictions)                   ->  list of metrics
#
# CLI:  Rscript R/corpus.R <out_dir> [--per N] [--centrality degree|eigenvector|betweenness] [--network ba|er|small_world] [--respondents M] [--seed S]
#
# Output layout (all CSV; dataset_id identifies the dataset in every file):
#   <out_dir>/blinded/README.txt            what the analysts get
#   <out_dir>/blinded/respondents.csv       one row per respondent
#   <out_dir>/blinded/ego_network.csv       one row per (respondent, contact) pair, if contact perceptions are recorded
#   <out_dir>/key.csv                       one row per dataset: process, parameters, population values
#   <out_dir>/key_details/nodes.csv         one row per agent of every population (same numbering as respondent_id)
#   <out_dir>/key_details/edges.csv         one row per tie of every population
#   <out_dir>/corpus.json                   the settings used

MECHANISM <- c("S1" = "genuine pluralistic ignorance", "S3" = "genuine pluralistic ignorance",
               "S2-pure" = "friendship paradox", "S2-mixed" = "friendship paradox", "S2-clairvoyant" = "friendship paradox")
SCENARIO_CODES <- c("S1", "S3", "S2-pure", "S2-mixed", "S2-clairvoyant")

default_corpus_settings <- function() {
  list(
    per_scenario = c("S1" = 30L, "S3" = 30L, "S2-pure" = 30L, "S2-mixed" = 30L, "S2-clairvoyant" = 30L),  # datasets per scenario version
    respondents = 150L,                                 # per dataset (capped at n)
    records = c("degree", "declaration", "ego_perceptions", "neighbor_degrees"),
    n_range = c(200L, 500L),
    network = "ba",                                     # network family for every dataset: ba | er | small_world
    ba_m = c(1L, 2L, 3L),                               # preferential attachment: contacts per new agent, drawn uniformly
    er_mean_degree = c(4, 8),                           # random graph: average number of contacts, drawn uniformly
    ws_k = c(4L, 6L, 8L),                               # small world: contacts per agent on the ring, drawn uniformly
    ws_beta = c(0.05, 0.3),                             # small world: share of ties rewired, drawn uniformly
    prevalence_range = c(0.55, 0.85),                   # pi0 (S1, S2) and psi (S3)
    conformity_a_range = c(3, 12),                       # c ~ beta(a, 1), the same range for every scenario; a is drawn per dataset
    lambda_range = c(2, 8),
    centrality = "degree",                              # one measure for the whole corpus: degree | eigenvector | betweenness
    max_attempt_factor = 100L,                           # give up on a scenario after this many attempts per accepted dataset
    write_state = TRUE,
    seed = 1L
  )
}

pick <- function(x) x[sample.int(length(x), 1L)]        # safe for length-1 vectors (sample(x, 1) is not)

draw_dataset_params <- function(code, s, attempt) {
  sc_i <- match(code, SCENARIO_CODES)
  sc <- if (startsWith(code, "S2")) "S2" else code
  set.seed(as.integer((s$seed * 1000003 + sc_i * 7919 + attempt) %% 2147483647))
  p <- list(scenario = sc, topology = s$network, n = pick(seq(s$n_range[1], s$n_range[2])), m = pick(s$ba_m))
  if (s$network == "er") p$mean_degree <- runif(1, s$er_mean_degree[1], s$er_mean_degree[2])
  if (s$network == "small_world") { p$k <- pick(s$ws_k); p$beta <- runif(1, s$ws_beta[1], s$ws_beta[2]) }
  prev <- runif(1, s$prevalence_range[1], s$prevalence_range[2])
  ca <- runif(1, s$conformity_a_range[1], s$conformity_a_range[2])
  if (code == "S1") { p$pi0 <- prev; p$c_dist <- "beta"; p$c_a <- ca; p$c_b <- 1 }
  if (code == "S3") { p$psi <- prev; p$c_dist <- "beta"; p$c_a <- ca; p$c_b <- 1 }
  if (sc == "S2") {
    p$pi0 <- prev; p$lambda <- runif(1, s$lambda_range[1], s$lambda_range[2]); p$centrality <- pick(s$centrality)
    p$pure <- code == "S2-pure"
    p$perception <- if (code == "S2-clairvoyant") "attitudes" else "declarations"
    if (!p$pure) { p$c_dist <- "beta"; p$c_a <- ca; p$c_b <- 1 }
  }
  # run seed; the survey and blinding draw from separate seeds (offsets far apart so streams never coincide)
  p$seed <- as.integer((s$seed * 100000 + sc_i * 10000 + attempt) %% 2147483647)
  p
}

# Recode attitudes 0 <-> 1 consistently across all columns of a survey.
flip_coding <- function(sv) {
  r <- sv$respondents; r$attitude <- 1L - r$attitude; r$declaration <- 1L - r$declaration; r$perceived_prevalence <- 1 - r$perceived_prevalence
  sv$respondents <- r
  if (!is.null(sv$ego_network)) { e <- sv$ego_network; e$perceived_neighbor_attitude <- 1L - e$perceived_neighbor_attitude; e$neighbor_attitude <- 1L - e$neighbor_attitude; sv$ego_network <- e }
  sv
}

# Anonymous identifiers: respondents are numbered 1..m in random order; any
# other node that appears as a contact (linked design only) gets a number above
# m, also in random order. Nothing about the population size leaks.
anonymize_ids <- function(sv, n) {
  egos <- sv$respondents$respondent_id; m <- length(egos)
  ids <- integer(n)
  ids[egos] <- sample.int(m)
  others <- setdiff(seq_len(n), egos)
  if (length(others)) ids[others] <- m + sample.int(length(others))
  sv$respondents$respondent_id <- ids[sv$respondents$respondent_id]
  sv$respondents <- sv$respondents[order(sv$respondents$respondent_id), ]
  if (!is.null(sv$ego_network)) {
    sv$ego_network$respondent_id <- ids[sv$ego_network$respondent_id]
    sv$ego_network$neighbor_id <- ids[sv$ego_network$neighbor_id]
    sv$ego_network <- sv$ego_network[order(sv$ego_network$respondent_id, sv$ego_network$neighbor_slot), ]
  }
  rownames(sv$respondents) <- NULL; rownames(sv$ego_network) <- NULL
  sv$id_map <- ids
  sv
}

# Does the sample itself show the pattern? (majority attitude among respondents,
# and its holders' mean perceived share of it below 1/2). Coding-invariant.
sample_shows_pattern <- function(resp) {
  # more than half of the respondents believe that a majority of their contacts hold the attitude opposite to their own
  same_perc <- ifelse(resp$attitude == 1L, resp$perceived_prevalence, 1 - resp$perceived_prevalence)
  mean(same_perc < 0.5, na.rm = TRUE) > 0.5
}

blinded_readme <- function(records, n_datasets) {
  cols <- c("respondent_id: identifier of the respondent, from 1 to the number of respondents, assigned at random",
            "attitude: the attitude the respondent reports holding privately (0 or 1)",
            "perceived_prevalence: the respondent's estimate of the share of the population holding attitude 1")
  if ("degree" %in% records) cols <- c(cols, "degree: the respondent's number of social contacts")
  if ("declaration" %in% records) cols <- c(cols, "declaration: the attitude the respondent expresses publicly (0 or 1)")
  ego <- NULL
  if ("ego_perceptions" %in% records) {
    ego <- c("respondent_id: the respondent making the report", "neighbor_slot: the position of the contact in the respondent's report (1, 2, ...); contacts are not identified and are listed in random order",
             "perceived_neighbor_attitude: the attitude the respondent believes this contact holds (0 or 1)")
    if ("neighbor_degrees" %in% records) ego <- c(ego, "neighbor_degree: this contact's number of social contacts")
    if ("linked" %in% records) ego <- c(ego, "neighbor_id: the contact's identifier (equal to respondent_id when the contact is also a respondent)", "neighbor_attitude: the attitude the contact reports holding privately")
  }
  paste0(
    "SURVEY DATASETS\n\n",
    sprintf("This folder contains %d datasets. Each dataset is a survey of a random sample of respondents from one population. The members of a population are connected by a social network, and each person observes the attitudes that their contacts express. ", n_datasets),
    "In every population, more than half of the people believe that most of their own contacts hold the attitude opposite to their own.\n\n",
    "DEFINITION\n\nA person is in a state of pluralistic ignorance when all of the following hold: the person believes that a majority of their own contacts hold the attitude opposite to the person's own; that belief is false (in fact at least half of their contacts share the person's attitude); and the person publicly expresses the opposite attitude. This can happen to holders of either attitude. The prevalence of pluralistic ignorance in a population is the share of its members who are in that state.\n\n",
    "Two different processes can produce the pattern described above:\n",
    "  (A) Genuine pluralistic ignorance: people's perceptions of their contacts' attitudes are systematically wrong, because contacts publicly express an attitude that differs from the one they privately hold.\n",
    "  (B) Network structure: well-connected people make up a large part of most people's contacts, and the well-connected people tend to hold the minority attitude. In some of these populations every person perceives their contacts' attitudes accurately; in others, people also conform their public expression to their contacts.\n\n",
    "TASK\n\nFor each dataset, estimate the prevalence of pluralistic ignorance in the population (a number between 0 and 1), and state which process produced the dataset if you can.\n\n",
    "Attitudes are coded 0 and 1. The coding is arbitrary and differs between datasets. Population sizes, sample sizes, and other properties differ between datasets.\n\n",
    "FILES\n\n",
    "respondents.csv, one row per respondent; the column dataset_id says which dataset the row belongs to:\n", paste0("  - ", cols, collapse = "\n"), "\n",
    if (!is.null(ego)) paste0("\nego_network.csv, one row per (respondent, contact) pair, for the contacts each respondent reported on; dataset_id and respondent_id match respondents.csv:\n", paste0("  - ", ego, collapse = "\n"), "\n") else "",
    "\nPREDICTIONS\n\nReturn a CSV file with the column dataset_id and any of: pi_prevalence (your estimate of the prevalence, between 0 and 1); prediction (\"genuine\" or \"structure\"); score (your estimated probability that the dataset was produced by genuine pluralistic ignorance).\n")
}

generate_blinded_corpus <- function(settings = default_corpus_settings(), out_dir, progress = NULL) {
  s <- modifyList(default_corpus_settings(), settings)
  accepted <- list(); attempts_by <- setNames(rep(0L, length(SCENARIO_CODES)), SCENARIO_CODES)
  total_target <- sum(s$per_scenario); done <- 0L
  for (sc in SCENARIO_CODES) {
    target <- if (sc %in% names(s$per_scenario)) s$per_scenario[[sc]] else 0L; got <- 0L; attempt <- 0L
    while (got < target && attempt < s$max_attempt_factor * target) {
      attempt <- attempt + 1L
      p <- draw_dataset_params(sc, s, attempt)
      st <- run_scenario(p); gt <- ground_truth(st)
      if (!gt$converged || !gt$pattern_present || gt$majority_share <= 0.5) next   # a strict majority that shows the pattern
      sv <- survey(st, s$respondents, survey_seed = p$seed + 500000L)
      if (!sample_shows_pattern(sv$respondents) || abs(mean(sv$respondents$attitude) - 0.5) < 1e-9) next   # the analysts must see the pattern too
      set.seed(p$seed + 700000L)
      flipped <- runif(1) < 0.5
      if (flipped) sv <- flip_coding(sv)
      sv <- anonymize_ids(sv, st$g$n)
      ee <- if ("ego_perceptions" %in% s$records) edge_end_estimates(sv$respondents, sv$ego_network)$net_misperception else NA_real_
      accepted[[length(accepted) + 1]] <- list(st = st, gt = gt, sv = sv, p = p, flipped = flipped, attempt = attempt, edge_end = ee)
      got <- got + 1L; done <- done + 1L
      if (!is.null(progress)) progress(done, total_target, sprintf("%s: %d of %d (attempt %d)", CLASS_NAMES[[scenario_label(p)]], got, target, attempt))
    }
    attempts_by[[sc]] <- attempt
  }
  if (length(accepted) == 0) stop("no dataset was accepted; increase the counts or widen the ranges")
  # blind the order
  set.seed(s$seed * 31L + 17L)
  ord <- sample.int(length(accepted))
  rows <- list(); resp_all <- list(); ego_all <- list(); nodes_all <- list(); edges_all <- list()
  for (k in seq_along(ord)) {
    x <- accepted[[ord[k]]]; id <- sprintf("dataset_%04d", k)
    ds <- apply_design(x$sv, s$records)
    resp_all[[k]] <- cbind(dataset_id = id, ds$respondents)
    if (!is.null(ds$ego_network)) ego_all[[k]] <- cbind(dataset_id = id, ds$ego_network)
    if (s$write_state) {
      st <- x$st; gt <- x$gt; ids <- x$sv$id_map; fl <- x$flipped
      perc <- if (identical(st$params$perception, "attitudes")) st$a else st$D
      pp <- vapply(st$g$nbrs, function(nb) if (length(nb)) mean(perc[nb]) else NA_real_, numeric(1))
      nd <- data.frame(dataset_id = id, node_id = ids,
                       attitude = if (fl) 1L - st$a else st$a, declaration = if (fl) 1L - st$D else st$D,
                       perceived_prevalence = if (fl) 1 - pp else pp, degree = st$g$deg, conformity = st$conformity,
                       sampled = seq_len(st$g$n) %in% x$sv$egos,
                       believes_own_attitude_is_local_minority = gt$apparent_individual, pluralistic_ignorance = gt$pi_individual)
      nodes_all[[k]] <- nd[order(nd$node_id), ]
      ed <- data.frame(dataset_id = id, node_a = ids[st$g$edges[, 1]], node_b = ids[st$g$edges[, 2]])
      edges_all[[k]] <- ed[order(ed$node_a, ed$node_b), ]
    }
    p <- x$p; gt <- x$gt
    rows[[k]] <- data.frame(
      dataset_id = id, mechanism = MECHANISM[[scenario_label(p)]], scenario = CLASS_NAMES[[scenario_label(p)]], scenario_code = scenario_label(p),
      perception = gt$perception,
      n = p$n, respondents = x$sv$m, network = NETWORK_NAMES[[p$topology]],
      links_per_new_agent = if (p$topology == "ba") p$m else NA_integer_, mean_degree = if (p$topology == "er") p$mean_degree else NA_real_,
      ring_contacts = if (p$topology == "small_world") p$k else NA_integer_, rewiring_share = if (p$topology == "small_world") p$beta else NA_real_,
      prevalence_parameter = if (p$scenario == "S3") p$psi else p$pi0,
      conformity = if (isTRUE(p$pure)) "none (c = 0)" else sprintf("beta(%.2f, 1)", p$c_a),
      lambda = if (p$scenario == "S2") p$lambda else NA_real_, centrality = if (p$scenario == "S2") p$centrality else NA_character_,
      seed = p$seed, attempt = x$attempt, attitude_coding_flipped = x$flipped,
      majority_attitude_as_coded = if (x$flipped) 1L - gt$majority_attitude else gt$majority_attitude,
      majority_share = gt$majority_share,
      apparent_share = gt$apparent_share, pattern_in_sample = sample_shows_pattern(x$sv$respondents),
      pi_prevalence = gt$pi_prevalence, pi_prevalence_attitude1 = if (x$flipped) gt$pi_prevalence_a0 else gt$pi_prevalence_a1,
      pi_prevalence_attitude0 = if (x$flipped) gt$pi_prevalence_a1 else gt$pi_prevalence_a0, pi_present = gt$pi_prevalence > 0,
      # decomposition terms in the majority-attitude frame (pluralistic ignorance is the negative direction), independent of coding
      misperception_term = majority_frame(gt)$misperception, structure_term = majority_frame(gt)$structure,
      misperception_gross = gt$misperception_gross,
      falsified_share = gt$falsified_share, deg_weighted_net_falsification = gt$deg_weighted_net_falsification,
      edge_end_test_in_sample_as_coded = x$edge_end, rounds = gt$rounds, stringsAsFactors = FALSE)
  }
  key <- do.call(rbind, rows)
  dir.create(file.path(out_dir, "blinded"), recursive = TRUE, showWarnings = FALSE)
  write.csv(do.call(rbind, resp_all), file.path(out_dir, "blinded", "respondents.csv"), row.names = FALSE)
  if (length(ego_all)) write.csv(do.call(rbind, ego_all), file.path(out_dir, "blinded", "ego_network.csv"), row.names = FALSE)
  writeLines(blinded_readme(s$records, nrow(key)), file.path(out_dir, "blinded", "README.txt"))
  write.csv(key, file.path(out_dir, "key.csv"), row.names = FALSE)
  if (s$write_state) {
    dir.create(file.path(out_dir, "key_details"), recursive = TRUE, showWarnings = FALSE)
    write.csv(do.call(rbind, nodes_all), file.path(out_dir, "key_details", "nodes.csv"), row.names = FALSE)
    write.csv(do.call(rbind, edges_all), file.path(out_dir, "key_details", "edges.csv"), row.names = FALSE)
  }
  jsonlite::write_json(c(s, list(created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), attempts = as.list(attempts_by), accepted = as.list(table(key$scenario_code)))),
                       file.path(out_dir, "corpus.json"), auto_unbox = TRUE, pretty = TRUE)
  attr(key, "attempts") <- attempts_by
  key
}

# Score a predictions file against the key. predictions: data.frame with
# dataset_id and any of pi_prevalence (estimate in [0, 1]), prediction
# ("genuine" / "structure", prefix match, case-insensitive), score = P(genuine).
score_predictions <- function(key, predictions) {
  pr <- predictions; names(pr) <- tolower(names(pr))
  stopifnot("dataset_id" %in% names(pr))
  has_prev <- "pi_prevalence" %in% names(pr); has_pred <- "prediction" %in% names(pr); has_score <- "score" %in% names(pr)
  if (!has_prev && !has_pred) stop("predictions need a pi_prevalence column or a prediction column")
  cols <- c("dataset_id", if (has_prev) "pi_prevalence", if (has_pred) "prediction", if (has_score) "score")
  m <- merge(key[, c("dataset_id", "mechanism", "scenario", "pi_prevalence", "pi_present")], pr[, cols], by = "dataset_id", suffixes = c("_true", "_est"))
  out <- list(n_scored = nrow(m), n_missing = nrow(key) - nrow(m))
  if (has_prev) {
    est <- as.numeric(m$pi_prevalence_est); tru <- m$pi_prevalence_true
    out$prevalence_mae <- mean(abs(est - tru)); out$prevalence_rmse <- sqrt(mean((est - tru)^2))
    out$prevalence_correlation <- if (sd(est) > 0 && sd(tru) > 0) cor(est, tru) else NA_real_
    out$prevalence_mae_by_scenario <- tapply(abs(est - tru), m$scenario, mean)
  }
  if (has_pred) {
    m$pred <- ifelse(grepl("^gen", tolower(trimws(m$prediction))), "genuine pluralistic ignorance",
              ifelse(grepl("^(str|fri|net)", tolower(trimws(m$prediction))), "friendship paradox", NA))
    ok <- !is.na(m$pred)
    out$accuracy <- mean(m$mechanism[ok] == m$pred[ok])
    out$confusion <- table(truth = m$mechanism, predicted = m$pred, useNA = "ifany")
    out$accuracy_by_scenario <- tapply(m$mechanism == m$pred, m$scenario, mean, na.rm = TRUE)
  }
  sc <- if (has_score) as.numeric(m$score) else if (has_prev) as.numeric(m$pi_prevalence_est) else NULL
  out$auc <- NA_real_
  if (!is.null(sc) && length(unique(m$mechanism)) == 2) {
    y <- m$mechanism == "genuine pluralistic ignorance"; r <- rank(sc); n1 <- sum(y); n0 <- sum(!y)
    out$auc <- (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  out
}

if (sys.nframe() == 0L && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) stop("usage: Rscript R/corpus.R <out_dir> [--per N] [--respondents M] [--seed S] [--no-state]")
  here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))))
  for (f in c("engine.R", "measure.R", "export.R")) source(file.path(here, f))
  s <- default_corpus_settings()
  if ("--per" %in% args) s$per_scenario[] <- as.integer(args[which(args == "--per") + 1])
  if ("--centrality" %in% args) s$centrality <- args[which(args == "--centrality") + 1]
  if ("--network" %in% args) s$network <- args[which(args == "--network") + 1]
  if ("--respondents" %in% args) s$respondents <- as.integer(args[which(args == "--respondents") + 1])
  if ("--seed" %in% args) s$seed <- as.integer(args[which(args == "--seed") + 1])
  if ("--no-state" %in% args) s$write_state <- FALSE
  key <- generate_blinded_corpus(s, args[1], progress = function(d, t, msg) if (d %% 10 == 0 || d == t) cat(sprintf("  %d / %d  %s\n", d, t, msg)))
  cat(sprintf("done: %d datasets in %s (attempts: %s)\n", nrow(key), args[1], paste(names(attr(key, "attempts")), attr(key, "attempts"), sep = "=", collapse = ", ")))
}
