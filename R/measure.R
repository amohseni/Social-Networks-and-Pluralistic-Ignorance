# Measurement: population ground truth, survey extraction, data configurations,
# and the statistic eta. Notation follows the problem statement: Y_i private
# attitude, X_i public declaration, P_i perceived population share holding 1,
# P_{N_i} perceived share among own contacts, N_i the contact set, d_i = |N_i|,
# mu_Y / mu_X / mu_P population means, Delta_G = mu_P - mu_Y. P_ij (the
# attitude i takes contact j to hold) and c_i (conformity) are additions.

# Population quantities and the decomposition of the perception discrepancy,
#   Delta_G = mean_i(mu_{Y,N_i} - mu_Y) + mean_i(P_{N_i} - mu_{Y,N_i})  [+ mean_i(P_i - P_{N_i}), zero here],
#   exposure bias + perception error, where mean_i(mu_{Y,N_i}) - mu_Y = Cov(Y, r)
# with r_j = sum_{i in N_j} 1/d_i the reach of j (mean reach is 1). Below f_i =
# mu_{Y,N_i} and fhat_i = P_{N_i}.
ground_truth <- function(st) {
  g <- st$g; n <- g$n; deg <- g$deg; Y <- st$Y; X <- st$X
  clair <- identical(st$params$perception, "attitudes")
  perc <- if (clair) Y else X                      # what agents take each neighbor to hold
  mu_Y <- mean(Y); mu_X <- mean(X)
  f    <- vapply(g$nbrs, function(nb) if (length(nb)) mean(Y[nb]) else NA_real_, numeric(1))
  fhat <- vapply(g$nbrs, function(nb) if (length(nb)) mean(perc[nb]) else NA_real_, numeric(1))
  ghat <- fhat
  inv_d <- ifelse(deg > 0, 1 / pmax(deg, 1), 0)
  r <- vapply(g$nbrs, function(nb) sum(inv_d[nb]), numeric(1))
  cov_ar <- mean(Y * r) - mean(Y) * mean(r)            # = mean_i(mu_{Y,N_i}) - mu_Y, the exposure bias
  fX <- vapply(g$nbrs, function(nb) if (length(nb)) mean(X[nb]) else NA_real_, numeric(1))
  Delta_F <- mean(fX, na.rm = TRUE) - mu_X                # exposure distortion of behavior (problem statement)
  sum_d <- sum(deg)
  mu_Y_edge <- sum(deg * Y) / sum_d                                       # E[DY]/E[D]: attitude of a contact reached along a random tie
  mu_PN_edge <- sum(deg * perc) / sum_d                                   # sum_i d_i P_{N_i} / sum_i d_i (census)
  mis <- mean(fhat - f, na.rm = TRUE)                     # perception error, net
  # gross perception error: mean over agents of the share of their contacts perceived wrongly
  wrong <- as.numeric(perc != Y)
  mis_gross <- mean(vapply(g$nbrs, function(nb) if (length(nb)) mean(wrong[nb]) else NA_real_, numeric(1)), na.rm = TRUE)
  mu_P <- mean(ghat, na.rm = TRUE)
  Delta_G <- mu_P - mu_Y
  E <- g$edges
  deg_assort <- if (nrow(E) > 1) suppressWarnings(cor(c(deg[E[, 1]], deg[E[, 2]]), c(deg[E[, 2]], deg[E[, 1]]))) else NA_real_
  edge_homophily <- if (nrow(E) > 0) mean(Y[E[, 1]] == Y[E[, 2]]) else NA_real_
  # Pluralistic ignorance is a state of an individual agent, judged against the
  # agent's own neighbors: the agent believes that a majority of its neighbors
  # hold the attitude opposite to its own; that belief is mistaken (at least half
  # of its neighbors share its attitude); and it declares the opposite attitude.
  # It can hold for agents of either attitude.
  same_true <- vapply(seq_len(n), function(i) { nb <- g$nbrs[[i]]; if (length(nb)) mean(Y[nb] == Y[i]) else NA_real_ }, numeric(1))
  same_perc <- vapply(seq_len(n), function(i) { nb <- g$nbrs[[i]]; if (length(nb)) mean(perc[nb] == Y[i]) else NA_real_ }, numeric(1))
  believes_minority <- !is.na(same_perc) & same_perc < 0.5      # believes most neighbors hold the opposite attitude
  mistaken <- !is.na(same_true) & same_true >= 0.5              # in fact at least half share the agent's attitude
  conforms <- X != Y
  apparent <- believes_minority                                 # the pattern, whatever the truth or the declaration
  pi_i <- believes_minority & mistaken & conforms               # pluralistic ignorance
  maj <- if (mu_Y >= 0.5) 1L else 0L
  list(
    n = n, n_edges = nrow(E), mean_degree = mean(deg), sd_degree = sd(deg), max_degree = max(deg),
    degree_assortativity = deg_assort, edge_homophily = edge_homophily,
    perception = if (clair) "attitudes" else "declarations",
    mu_Y = mu_Y, mu_X = mu_X, mu_Y_edge = mu_Y_edge, mu_PN_edge = mu_PN_edge, mu_P = mu_P,
    Delta_G = Delta_G, perception_error = mis, exposure_bias = cov_ar, perception_error_gross = mis_gross, Delta_F = Delta_F,
    decomposition_residual = Delta_G - (mis + cov_ar),
    falsified_share = mean(X != Y),
    falsified_share_a1 = if (any(Y == 1)) mean(X[Y == 1] != 1) else NA_real_,
    falsified_share_a0 = if (any(Y == 0)) mean(X[Y == 0] != 0) else NA_real_,
    eta = mu_PN_edge - mu_Y_edge,                          # the degree-weighted perception error (JC's statistic)
    majority_attitude = maj,
    majority_share = max(mu_Y, 1 - mu_Y),
    apparent_share = mean(apparent),                       # share of all agents who believe their attitude is the minority among their neighbors
    apparent_share_a1 = if (any(Y == 1)) mean(apparent[Y == 1]) else NA_real_,
    apparent_share_a0 = if (any(Y == 0)) mean(apparent[Y == 0]) else NA_real_,
    pattern_present = isTRUE(mean(apparent) > 0.5),
    pi_prevalence = mean(pi_i),                            # share of all agents in pluralistic ignorance
    pi_prevalence_a1 = if (any(Y == 1)) mean(pi_i[Y == 1]) else NA_real_,
    pi_prevalence_a0 = if (any(Y == 0)) mean(pi_i[Y == 0]) else NA_real_,
    pi_individual = pi_i, apparent_individual = apparent,
    mean_conformity = mean(st$conformity),
    n_internalized = st$n_internalized %||% 0L,
    homophily_swaps = st$g$homophily_swaps %||% 0L,
    converged = isTRUE(st$converged), rounds = st$rounds
  )
}

# Terms in the majority-attitude frame (sign-flipped when the majority attitude
# is 0), so pluralistic ignorance is always the negative direction.
majority_frame <- function(gt) {
  s <- if (gt$mu_Y >= 0.5) 1 else -1
  list(exposure_bias = s * gt$exposure_bias, perception_error = s * gt$perception_error, Delta_G = s * gt$Delta_G)
}

# ------------------------------------------------------------------ data configurations
#
# The problem statement (Pluralistic Ignorance and the Friendship Paradox,
# "Five data configurations", board of 27 September) names what a study
# records about each respondent i. The same letters are used here, with the
# column names the files carry:
#   Y_i   private attitude ................ attitude
#   X_i   public behavior ................. declaration
#   P_i   perceived population attitude ... perceived_prevalence
#   P_Ni  perceived attitude of own contacts (one extra survey item) ... perceived_contacts_mean
#   d_i   number of contacts .............. degree
#   contacts' (X_j, Y_j, d_j) ............. neighbor_declaration, neighbor_attitude, neighbor_degree (ego_network.csv)
#   A     the global network .............. network_edges.csv
# In this model respondents extrapolate naively from their contacts, so
# P_i = P_Ni unless extrapolation noise is added at survey time.
CONFIGS <- list(
  C1  = list(name = "C1: attitude and perceived prevalence",            resp = character(0),               ego = "none",      network = FALSE, census = FALSE),
  C2  = list(name = "C2: C1 + own declaration",                         resp = "declaration",              ego = "none",      network = FALSE, census = FALSE),
  C3  = list(name = "C3: C2 + number of contacts",                      resp = c("declaration", "degree"), ego = "none",      network = FALSE, census = FALSE),
  C3a = list(name = "C3a: C3 + perceived attitude of each contact, contacts anonymous", resp = c("declaration", "degree"), ego = "anonymous", network = FALSE, census = FALSE),
  C4  = list(name = "C4: C3 + ego network (contacts identified, with their own survey answers)", resp = c("declaration", "degree"), ego = "linked", network = FALSE, census = FALSE),
  C5  = list(name = "C5: C4 for every agent, plus the global network (a census)", resp = c("declaration", "degree"), ego = "linked", network = TRUE, census = TRUE)
)
CONFIG_CODES <- names(CONFIGS)
CONFIG_NAMES <- vapply(CONFIGS, `[[`, character(1), "name")

# One line per configuration for READMEs and the GUI.
config_summary <- function(code, add_pn = FALSE) {
  cf <- CONFIGS[[code]]
  resp <- c("respondent_id", "attitude", "perceived_prevalence", if (add_pn) "perceived_contacts_mean", cf$resp)
  files <- sprintf("respondents.csv (%s)", paste(resp, collapse = ", "))
  if (cf$ego == "anonymous") files <- c(files, "ego_network.csv (respondent_id, neighbor_slot, perceived_neighbor_attitude)")
  if (cf$ego == "linked") files <- c(files, "ego_network.csv (respondent_id, neighbor_slot, neighbor_id, perceived_neighbor_attitude, neighbor_declaration, neighbor_degree, neighbor_attitude, neighbor_responded)")
  if (cf$network) files <- c(files, "network_edges.csv (node_a, node_b)")
  list(code = code, name = cf$name, files = files, census = cf$census)
}

# Legacy per-column survey options (sweep.R and older tests); the configurations above are the interface.
SURVEY_OPTIONS <- c(
  degree            = "Respondent's number of contacts (degree)",
  declaration       = "Respondent's own public declaration",
  ego_perceptions   = "Perceived attitude of each contact (contacts anonymous)",
  neighbor_degrees  = "Each contact's number of contacts",
  linked            = "Each contact's identity and true attitude (linked design)"
)

# Survey of a population. A uniform sample of m respondents (egos) is drawn
# without replacement; records are built for EVERY agent so that the census
# configuration (C5) and the sampled ones (C1 to C4) describe the same
# population with the same identifiers. Column `sampled` marks the egos.
#   perceived_contacts_mean: share of contacts the agent takes to hold 1 (P_Ni), exact;
#   perceived_prevalence:    the agent's estimate for the population (P_i) = P_Ni plus
#                            optional extrapolation noise (sd noise_sd, clipped to [0, 1]).
#   alter_response_rate: share of non-sampled agents who answer the survey when named as
#                        a contact; a contact that does not answer has neighbor_attitude and
#                        neighbor_degree NA in the linked design. Sampled agents always answer.
survey <- function(st, m, survey_seed = 1L, noise_sd = 0, alter_response_rate = 1) {
  set.seed(as.integer(survey_seed))
  n <- st$g$n; m <- min(as.integer(m), n)
  perc <- if (identical(st$params$perception, "attitudes")) st$Y else st$X   # clairvoyant agents perceive true attitudes
  egos <- sample.int(n, m)
  sampled <- seq_len(n) %in% egos
  responded <- sampled | (runif(n) < alter_response_rate)
  recs <- lapply(seq_len(n), function(i) {
    nb <- st$g$nbrs[[i]]
    slot <- if (length(nb) > 1) sample.int(length(nb)) else seq_along(nb)   # shuffle: contacts listed in random order
    nb <- nb[slot]
    pn <- if (length(nb)) mean(perc[nb]) else NA_real_
    ghat <- if (noise_sd > 0 && !is.na(pn)) min(1, max(0, pn + rnorm(1, 0, noise_sd))) else pn
    list(id = i, Y = st$Y[i], X = st$X[i], d = length(nb), pn = pn, ghat = ghat, nb = nb)
  })
  respondents <- data.frame(
    respondent_id = vapply(recs, `[[`, integer(1), "id"),
    attitude = vapply(recs, `[[`, integer(1), "Y"),
    perceived_prevalence = vapply(recs, `[[`, numeric(1), "ghat"),
    perceived_contacts_mean = vapply(recs, `[[`, numeric(1), "pn"),
    degree = vapply(recs, `[[`, integer(1), "d"),
    declaration = vapply(recs, `[[`, integer(1), "X"),
    sampled = sampled, responded = responded)
  ego_network <- do.call(rbind, lapply(recs, function(r) if (r$d > 0) data.frame(
    respondent_id = r$id, neighbor_slot = seq_len(r$d), neighbor_id = r$nb,
    perceived_neighbor_attitude = perc[r$nb], neighbor_declaration = st$X[r$nb],
    neighbor_degree = st$g$deg[r$nb], neighbor_attitude = st$Y[r$nb],
    neighbor_responded = responded[r$nb]) else NULL))
  list(egos = egos, m = m, n = n, survey_seed = survey_seed, noise_sd = noise_sd, alter_response_rate = alter_response_rate,
       respondents = respondents, ego_network = ego_network, edges = st$g$edges)
}

# The sampled respondents only (the view every configuration but C5 starts from).
survey_sample <- function(sv) {
  r <- sv$respondents[sv$respondents$sampled, , drop = FALSE]
  e <- if (!is.null(sv$ego_network)) sv$ego_network[sv$ego_network$respondent_id %in% r$respondent_id, , drop = FALSE] else NULL
  list(respondents = r, ego_network = e)
}

# Apply a data configuration: return the files the analysts receive for it.
#   respondents: one row per respondent (every agent for C5);
#   ego_network: one row per (respondent, contact) pair, for configurations that record contacts;
#   network_edges: the global network (C5).
apply_config <- function(sv, code = "C1", add_pn = FALSE) {
  cf <- CONFIGS[[code]]
  if (is.null(cf)) stop("unknown data configuration: ", code)
  v <- if (cf$census) list(respondents = sv$respondents, ego_network = sv$ego_network) else survey_sample(sv)
  resp_cols <- c("respondent_id", "attitude", "perceived_prevalence", if (add_pn) "perceived_contacts_mean", cf$resp)
  out <- list(respondents = v$respondents[, resp_cols, drop = FALSE])
  rownames(out$respondents) <- NULL
  if (cf$ego != "none" && !is.null(v$ego_network)) {
    e <- v$ego_network
    if (cf$ego == "anonymous") {
      out$ego_network <- e[, c("respondent_id", "neighbor_slot", "perceived_neighbor_attitude"), drop = FALSE]
    } else {
      if (!cf$census) {                       # a contact that did not answer has no own answers
        e$neighbor_attitude[!e$neighbor_responded] <- NA_integer_
        e$neighbor_degree[!e$neighbor_responded] <- NA_integer_
      } else e$neighbor_responded <- TRUE      # a census: everyone answered
      out$ego_network <- e[, c("respondent_id", "neighbor_slot", "neighbor_id", "perceived_neighbor_attitude", "neighbor_declaration",
                               "neighbor_degree", "neighbor_attitude", "neighbor_responded"), drop = FALSE]
    }
    rownames(out$ego_network) <- NULL
  }
  if (cf$network) out$network_edges <- data.frame(node_a = sv$edges[, 1], node_b = sv$edges[, 2])
  out
}

# Legacy: apply a per-column survey design (sampled respondents only).
apply_design <- function(sv, include = names(SURVEY_OPTIONS)) {
  v <- survey_sample(sv)
  resp_cols <- c("respondent_id", "attitude", "perceived_prevalence",
                 if ("degree" %in% include) "degree", if ("declaration" %in% include) "declaration")
  out <- list(respondents = v$respondents[, resp_cols, drop = FALSE])
  rownames(out$respondents) <- NULL
  if ("ego_perceptions" %in% include && !is.null(v$ego_network)) {
    ego_cols <- c("respondent_id", "neighbor_slot", "perceived_neighbor_attitude",
                  if ("neighbor_degrees" %in% include) "neighbor_degree",
                  if ("linked" %in% include) c("neighbor_id", "neighbor_attitude"))
    out$ego_network <- v$ego_network[, ego_cols, drop = FALSE]
    rownames(out$ego_network) <- NULL
  }
  out
}

# eta (the problem statement's statistic, JC 2026-10-06): the degree-weighted
# perception error sum_i d_i (P_{N_i} - Y_i) / sum_i d_i, estimated as the mean
# of P_ij over all reported contacts minus the degree-weighted mean of the
# respondents' own Y_i. Needs only configuration C3a. In a census it equals
# mu_PN_edge - mu_Y_edge exactly, and is exactly zero when every agent
# perceives its contacts accurately.
edge_end_estimates <- function(respondents, ego_network) {
  d <- table(factor(ego_network$respondent_id, levels = respondents$respondent_id))
  d <- as.numeric(d)
  sum_d <- sum(d)
  q_hat <- sum(ego_network$perceived_neighbor_attitude) / sum_d
  mu_Y_edge_hat <- sum(d * respondents$attitude) / sum_d
  list(q_hat = q_hat, mu_Y_edge_hat = mu_Y_edge_hat, eta_hat = q_hat - mu_Y_edge_hat)
}

edge_end_bootstrap <- function(respondents, ego_network, B = 500L, seed = 1L) {
  set.seed(seed)
  ids <- respondents$respondent_id
  d <- as.numeric(table(factor(ego_network$respondent_id, levels = ids)))
  q <- as.numeric(tapply(ego_network$perceived_neighbor_attitude, factor(ego_network$respondent_id, levels = ids), sum))
  q[is.na(q)] <- 0
  Y <- respondents$attitude
  stat <- function(idx) sum(q[idx]) / sum(d[idx]) - sum(d[idx] * Y[idx]) / sum(d[idx])
  boots <- replicate(B, stat(sample.int(length(ids), replace = TRUE)))
  list(estimate = stat(seq_along(ids)), ci_lower = unname(quantile(boots, 0.025)), ci_upper = unname(quantile(boots, 0.975)), B = B)
}
