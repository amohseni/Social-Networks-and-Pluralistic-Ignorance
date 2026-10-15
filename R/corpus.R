# Generated corpus: datasets in which pluralistic ignorance obtains, produced by
# either process, delivered as files for the analysts plus a separate key.
#
#   generate_blinded_corpus(settings, out_dir, progress)  ->  key data frame
#   score_predictions(key, predictions)                   ->  list of metrics
#
# CLI:  Rscript R/corpus.R <out_dir> [--per N] [--centrality degree|eigenvector|betweenness] [--network ba|er|small_world] [--respondents M] [--seed S] [--configs C1,C3,C4] [--pn]
#
# Output layout (all CSV; dataset_id identifies the dataset in every file):
#   <out_dir>/for_analysts/README.txt              overview: the configurations are nested views of the same datasets
#   <out_dir>/for_analysts/<C>/README.txt          codebook for configuration C (C1, C2, C3, C3a, C4, C5)
#   <out_dir>/for_analysts/<C>/respondents.csv     one row per respondent (every agent for C5)
#   <out_dir>/for_analysts/<C>/ego_network.csv     one row per (respondent, contact) pair (C3a, C4, C5)
#   <out_dir>/for_analysts/<C>/network_edges.csv   one row per tie (C5)
#   <out_dir>/key/key.csv                          one row per dataset: process, parameters, population values
#   <out_dir>/key/nodes.csv                        one row per agent of every population (same numbering as respondent_id)
#   <out_dir>/key/edges.csv                        one row per tie of every population
#   <out_dir>/key/corpus.json                      the settings used
#   <out_dir>/key/README.txt                       what the key files contain

MECHANISM <- c("S1" = "genuine pluralistic ignorance", "S3" = "genuine pluralistic ignorance",
               "S2-pure" = "friendship paradox", "S2-mixed" = "friendship paradox", "S2-clairvoyant" = "friendship paradox")
SCENARIO_CODES <- c("S1", "S3", "S2-pure", "S2-mixed", "S2-clairvoyant")

default_corpus_settings <- function() {
  list(
    per_scenario = c("S1" = 30L, "S3" = 30L, "S2-pure" = 30L, "S2-mixed" = 30L, "S2-clairvoyant" = 30L),  # datasets per scenario version
    respondents = 150L,                                 # per dataset (capped at n)
    configs = CONFIG_CODES,                              # data configurations to write (C1 ... C5); every dataset is written in each
    add_pn = FALSE,                                      # add the perceived-contacts item P_Ni (perceived_contacts_mean) to every configuration
    noise_sd = 0,                                        # extrapolation noise: sd added to perceived_prevalence (P_i = P_Ni + noise)
    alter_response_rate = 1,                             # C4: share of non-sampled contacts who answer the survey
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
  r <- sv$respondents
  r$attitude <- 1L - r$attitude; r$declaration <- 1L - r$declaration
  r$perceived_prevalence <- 1 - r$perceived_prevalence; r$perceived_contacts_mean <- 1 - r$perceived_contacts_mean
  sv$respondents <- r
  if (!is.null(sv$ego_network)) {
    e <- sv$ego_network
    e$perceived_neighbor_attitude <- 1L - e$perceived_neighbor_attitude
    e$neighbor_declaration <- 1L - e$neighbor_declaration
    e$neighbor_attitude <- 1L - e$neighbor_attitude
    sv$ego_network <- e
  }
  sv
}

# Anonymous identifiers: sampled respondents are numbered 1..m in random order;
# every other agent gets a number above m, also in random order. The same
# numbering is used in every configuration, in the key, and in the network
# files, so the configurations can be handed out one at a time and compared.
anonymize_ids <- function(sv, n) {
  egos <- sv$egos; m <- length(egos)
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
  if (!is.null(sv$edges)) {
    E <- cbind(ids[sv$edges[, 1]], ids[sv$edges[, 2]])
    E <- cbind(pmin(E[, 1], E[, 2]), pmax(E[, 1], E[, 2]))
    sv$edges <- E[order(E[, 1], E[, 2]), , drop = FALSE]
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

DEFINITION_TEXT <- paste0(
  "DEFINITION\n\nA person is in a state of pluralistic ignorance when all of the following hold: the person believes that a majority of their own contacts hold the attitude opposite to the person's own; that belief is false (in fact at least half of their contacts share the person's attitude); and the person publicly expresses the opposite attitude. This can happen to holders of either attitude. The prevalence of pluralistic ignorance in a population is the share of its members who are in that state.\n\n",
  "Two different processes can produce the pattern described above:\n",
  "  (A) Genuine pluralistic ignorance: people's perceptions of their contacts' attitudes are systematically wrong, because contacts publicly express an attitude that differs from the one they privately hold.\n",
  "  (B) Network structure: well-connected people make up a large part of most people's contacts, and the well-connected people tend to hold the minority attitude. In some of these populations every person perceives their contacts' attitudes accurately; in others, people also conform their public expression to their contacts.\n\n",
  "TASK\n\nFor each dataset, estimate the prevalence of pluralistic ignorance in the population (a number between 0 and 1), and state which process produced the dataset if you can.\n\n",
  "Attitudes are coded 0 and 1. The coding is arbitrary and differs between datasets. Population sizes, sample sizes, and other properties differ between datasets.\n\n")

PREDICTIONS_TEXT <- "PREDICTIONS\n\nReturn a CSV file with the column dataset_id and any of: pi_prevalence (your estimate of the prevalence, between 0 and 1); prediction (\"genuine\" or \"structure\"); score (your estimated probability that the dataset was produced by genuine pluralistic ignorance).\n"

# Column descriptions, by file, for a configuration.
config_codebook <- function(code, add_pn = FALSE, noise_sd = 0, alter_response_rate = 1) {
  cf <- CONFIGS[[code]]
  resp <- c("respondent_id: identifier of the respondent, assigned at random",
            "attitude: the attitude the respondent reports holding privately (0 or 1)",
            "perceived_prevalence: the respondent's estimate of the share of the whole population holding attitude 1")
  if (add_pn) resp <- c(resp, "perceived_contacts_mean: the respondent's estimate of the share of their own contacts holding attitude 1")
  if ("declaration" %in% cf$resp) resp <- c(resp, "declaration: the attitude the respondent expresses publicly (0 or 1)")
  if ("degree" %in% cf$resp) resp <- c(resp, "degree: the respondent's number of social contacts")
  ego <- NULL
  if (cf$ego == "anonymous") ego <- c(
    "respondent_id: the respondent making the report",
    "neighbor_slot: the position of the contact in the respondent's report (1, 2, ...); contacts are not identified and are listed in random order",
    "perceived_neighbor_attitude: the attitude the respondent believes this contact holds (0 or 1)")
  if (cf$ego == "linked") ego <- c(
    "respondent_id: the respondent making the report",
    "neighbor_slot: the position of the contact in the respondent's report (1, 2, ...), in random order",
    "neighbor_id: the contact's identifier (equal to respondent_id when the contact is also a respondent); a contact named by several respondents has the same identifier in each report",
    "perceived_neighbor_attitude: the attitude the respondent believes this contact holds (0 or 1)",
    "neighbor_declaration: the attitude the contact expresses publicly (0 or 1), as observed",
    "neighbor_degree: the contact's number of social contacts, from the contact's own survey answer (empty if the contact did not answer)",
    "neighbor_attitude: the attitude the contact reports holding privately, from the contact's own survey answer (empty if the contact did not answer)",
    "neighbor_responded: whether the contact answered the survey (TRUE or FALSE)")
  net <- if (cf$network) c("node_a, node_b: the two agents joined by a tie; identifiers match respondent_id") else NULL
  list(respondents = resp, ego_network = ego, network_edges = net)
}

config_readme <- function(code, n_datasets, add_pn = FALSE, noise_sd = 0, alter_response_rate = 1) {
  cf <- CONFIGS[[code]]; cb <- config_codebook(code, add_pn, noise_sd, alter_response_rate)
  what <- switch(code,
    C1 = "Each respondent reports their own attitude and their estimate of the population's attitude. This is the standard survey design.",
    C2 = "As C1, plus the attitude each respondent expresses publicly.",
    C3 = "As C2, plus each respondent's number of social contacts.",
    C3a = "As C3, plus the respondent's belief about the attitude of each of their contacts. Contacts are not identified.",
    C4 = "As C3, plus an ego network: each respondent's contacts are identified, and the contacts' own survey answers are included when the contact answered. Contacts named by several respondents have the same identifier in each report.",
    C5 = "A census: every member of the population answers the survey, every ego network is complete, and the full network of ties is included. This configuration is the reference point: nothing about the population is hidden except which process produced it.")
  extra <- c(
    if (add_pn) "The item perceived_contacts_mean asks the respondent about their own contacts; perceived_prevalence asks about the whole population." else NULL,
    if (noise_sd > 0) sprintf("Estimates of the population share carry reporting noise (standard deviation %.3f, kept within 0 and 1).", noise_sd) else NULL,
    if (cf$ego == "linked" && !cf$census && alter_response_rate < 1) sprintf("A contact who is not a respondent answers the survey with probability %.2f; contacts who did not answer have empty own-answer columns.", alter_response_rate) else NULL)
  paste0(
    "SURVEY DATASETS, CONFIGURATION ", code, "\n\n", cf$name, "\n\n",
    sprintf("This folder contains %d datasets. Each dataset is a survey of %s from one population. The members of a population are connected by a social network, and each person observes the attitudes that their contacts express. ",
            n_datasets, if (cf$census) "every member" else "a random sample of respondents"),
    "In every population, more than half of the people believe that most of their own contacts hold the attitude opposite to their own.\n\n",
    "WHAT IS RECORDED\n\n", what, "\n", if (length(extra)) paste0(paste(extra, collapse = " "), "\n") else "", "\n",
    DEFINITION_TEXT,
    "FILES\n\n",
    "respondents.csv, one row per respondent; the column dataset_id says which dataset the row belongs to:\n", paste0("  - ", cb$respondents, collapse = "\n"), "\n",
    if (!is.null(cb$ego_network)) paste0("\nego_network.csv, one row per (respondent, contact) pair; dataset_id and respondent_id match respondents.csv:\n", paste0("  - ", cb$ego_network, collapse = "\n"), "\n") else "",
    if (!is.null(cb$network_edges)) paste0("\nnetwork_edges.csv, one row per tie in the population; dataset_id matches respondents.csv:\n", paste0("  - ", cb$network_edges, collapse = "\n"), "\n") else "",
    "\n", PREDICTIONS_TEXT)
}

overview_readme <- function(configs, n_datasets, add_pn = FALSE) {
  lines <- vapply(configs, function(code) sprintf("  %-4s %s", code, CONFIGS[[code]]$name), character(1))
  paste0(
    "SURVEY DATASETS\n\n",
    sprintf("This package contains %d datasets, each written in %d data configuration%s. A configuration is what a study records about each respondent. The configurations are nested: each one adds information to the one before, and all of them describe the same %d datasets, with the same dataset_id and the same respondent_id throughout, so an analysis can start from the sparsest configuration and add information step by step.\n\n",
            n_datasets, length(configs), if (length(configs) > 1) "s" else "", n_datasets),
    paste(lines, collapse = "\n"), "\n\n",
    "Each configuration is a folder with its own README.txt (a codebook for its columns) and its CSV files.\n\n",
    if (add_pn) "Every configuration also carries the item perceived_contacts_mean, the respondent's estimate of the share of their own contacts holding attitude 1.\n\n" else "",
    DEFINITION_TEXT, PREDICTIONS_TEXT)
}

key_readme <- function() paste0(
  "KEY (keep apart from the analysts)\n\n",
  "key.csv: one row per dataset. mechanism says which process produced the dataset; scenario and scenario_code name the version; the remaining columns give every parameter, the population values (prevalence of pluralistic ignorance overall and by attitude as coded, the share who believe they are in the local minority, the share declaring against their attitude, the exposure bias and the perception error, Delta_F, eta for the population), whether the attitude coding was reversed, and eta computed on the respondents (eta_sample_as_coded).\n\n",
  "nodes.csv: one row per agent of every population, numbered as respondent_id in the analysts' files: Y (attitude), X (declaration), P (perceived prevalence), P_N (perceived share among own contacts), d (degree), c (conformity parameter), whether the agent was sampled, whether it answered when named as a contact, whether it believes its attitude is the local minority, and whether it is in a state of pluralistic ignorance.\n\n",
  "edges.csv: one row per tie of every population, with the same numbering.\n\n",
  "corpus.json: the settings used to generate the corpus, with the seed; the same settings and seed reproduce it exactly.\n")

write_config_files <- function(dir, accepted_ord, configs, add_pn, noise_sd, alter_response_rate) {
  # accepted_ord: list of accepted datasets in blinded order, each with $sv and $id
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(overview_readme(configs, length(accepted_ord), add_pn), file.path(dir, "README.txt"))
  for (code in configs) {
    cdir <- file.path(dir, code); dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
    parts <- lapply(accepted_ord, function(x) { ds <- apply_config(x$sv, code, add_pn); lapply(ds, function(d) cbind(dataset_id = x$id, d)) })
    for (f in c("respondents", "ego_network", "network_edges")) {
      tabs <- Filter(Negate(is.null), lapply(parts, `[[`, f))
      if (length(tabs)) write.csv(do.call(rbind, tabs), file.path(cdir, paste0(f, ".csv")), row.names = FALSE, na = "")
    }
    writeLines(config_readme(code, length(accepted_ord), add_pn, noise_sd, alter_response_rate), file.path(cdir, "README.txt"))
  }
  invisible(NULL)
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
      sv <- survey(st, s$respondents, survey_seed = p$seed + 500000L, noise_sd = s$noise_sd, alter_response_rate = s$alter_response_rate)
      smp <- survey_sample(sv)
      if (!sample_shows_pattern(smp$respondents) || abs(mean(smp$respondents$attitude) - 0.5) < 1e-9) next   # the analysts must see the pattern too
      set.seed(p$seed + 700000L)
      flipped <- runif(1) < 0.5
      if (flipped) sv <- flip_coding(sv)
      sv <- anonymize_ids(sv, st$g$n)
      smp <- survey_sample(sv)
      ee <- edge_end_estimates(smp$respondents, smp$ego_network)$eta_hat
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
  rows <- list(); nodes_all <- list(); edges_all <- list(); ordered <- list()
  for (k in seq_along(ord)) {
    x <- accepted[[ord[k]]]; id <- sprintf("dataset_%04d", k)
    ordered[[k]] <- list(sv = x$sv, id = id)
    if (s$write_state) {
      st <- x$st; gt <- x$gt; ids <- x$sv$id_map; fl <- x$flipped
      r <- x$sv$respondents                                   # already flipped and renumbered; one row per agent, ordered by id
      nd <- data.frame(dataset_id = id, node_id = r$respondent_id,
                       attitude = r$attitude, declaration = r$declaration,
                       perceived_prevalence = r$perceived_prevalence, perceived_contacts_mean = r$perceived_contacts_mean,
                       degree = r$degree, conformity = st$conformity[order(ids)], sampled = r$sampled, responded = r$responded,
                       believes_own_attitude_is_local_minority = gt$apparent_individual[order(ids)], pluralistic_ignorance = gt$pi_individual[order(ids)])
      nodes_all[[k]] <- nd
      edges_all[[k]] <- data.frame(dataset_id = id, node_a = x$sv$edges[, 1], node_b = x$sv$edges[, 2])
    }
    p <- x$p; gt <- x$gt
    rows[[k]] <- data.frame(
      dataset_id = id, mechanism = MECHANISM[[scenario_label(p)]], scenario = CLASS_NAMES[[scenario_label(p)]], scenario_code = scenario_label(p),
      perception = gt$perception,
      N = p$n, respondents = x$sv$m, network = NETWORK_NAMES[[p$topology]], configurations = paste(s$configs, collapse = " "),
      links_per_new_agent = if (p$topology == "ba") p$m else NA_integer_, mean_degree = if (p$topology == "er") p$mean_degree else NA_real_,
      ring_contacts = if (p$topology == "small_world") p$k else NA_integer_, rewiring_share = if (p$topology == "small_world") p$beta else NA_real_,
      prevalence_parameter = if (p$scenario == "S3") p$psi else p$pi0,
      conformity = if (isTRUE(p$pure)) "none (c = 0)" else sprintf("beta(%.2f, 1)", p$c_a),
      lambda = if (p$scenario == "S2") p$lambda else NA_real_, centrality = if (p$scenario == "S2") p$centrality else NA_character_,
      seed = p$seed, attempt = x$attempt, attitude_coding_flipped = x$flipped,
      majority_attitude_as_coded = if (x$flipped) 1L - gt$majority_attitude else gt$majority_attitude,
      majority_share = gt$majority_share,
      apparent_share = gt$apparent_share, pattern_in_sample = sample_shows_pattern(survey_sample(x$sv)$respondents),
      pi_prevalence = gt$pi_prevalence, pi_prevalence_attitude1 = if (x$flipped) gt$pi_prevalence_a0 else gt$pi_prevalence_a1,
      pi_prevalence_attitude0 = if (x$flipped) gt$pi_prevalence_a1 else gt$pi_prevalence_a0, pi_present = gt$pi_prevalence > 0,
      # decomposition terms in the majority-attitude frame (pluralistic ignorance is the negative direction), independent of coding
      perception_error = majority_frame(gt)$perception_error, exposure_bias = majority_frame(gt)$exposure_bias,
      perception_error_gross = gt$perception_error_gross, Delta_F = gt$Delta_F,
      falsified_share = gt$falsified_share, eta = gt$eta,
      eta_sample_as_coded = x$edge_end, rounds = gt$rounds, stringsAsFactors = FALSE)
  }
  key <- do.call(rbind, rows)
  write_config_files(file.path(out_dir, "for_analysts"), ordered, s$configs, s$add_pn, s$noise_sd, s$alter_response_rate)
  kdir <- file.path(out_dir, "key"); dir.create(kdir, recursive = TRUE, showWarnings = FALSE)
  write.csv(key, file.path(kdir, "key.csv"), row.names = FALSE)
  writeLines(key_readme(), file.path(kdir, "README.txt"))
  if (s$write_state) {
    write.csv(do.call(rbind, nodes_all), file.path(kdir, "nodes.csv"), row.names = FALSE)
    write.csv(do.call(rbind, edges_all), file.path(kdir, "edges.csv"), row.names = FALSE)
  }
  jsonlite::write_json(c(s, list(created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), attempts = as.list(attempts_by), accepted = as.list(table(key$scenario_code)))),
                       file.path(kdir, "corpus.json"), auto_unbox = TRUE, pretty = TRUE)
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
  if ("--configs" %in% args) s$configs <- strsplit(args[which(args == "--configs") + 1], ",")[[1]]
  if ("--pn" %in% args) s$add_pn <- TRUE
  key <- generate_blinded_corpus(s, args[1], progress = function(d, t, msg) if (d %% 10 == 0 || d == t) cat(sprintf("  %d / %d  %s\n", d, t, msg)))
  cat(sprintf("done: %d datasets in %s (attempts: %s)\n", nrow(key), args[1], paste(names(attr(key, "attempts")), attr(key, "attempts"), sep = "=", collapse = ", ")))
}
