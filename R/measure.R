# Measurement: population ground truth, survey extraction, and the edge-end
# estimator. Vocabulary: attitude (private), declaration (public), perceived
# prevalence (an agent's estimate of the population share holding attitude 1,
# taken to be the share of its neighbors declaring 1).

# Population quantities and the two-term decomposition of the perception gap:
#   E[ghat] - p = E[fhat - f]  (misperception)  +  Cov(A, r)  (structure),
# with f_i the true share of i's neighbors holding attitude 1, fhat_i the share
# declaring 1, and r_j = sum_{i in N(j)} 1/d_i the reach of j (mean reach is 1).
ground_truth <- function(st) {
  g <- st$g; n <- g$n; deg <- g$deg; a <- st$a; D <- st$D
  p <- mean(a)
  f    <- vapply(g$nbrs, function(nb) if (length(nb)) mean(a[nb]) else NA_real_, numeric(1))
  fhat <- vapply(g$nbrs, function(nb) if (length(nb)) mean(D[nb]) else NA_real_, numeric(1))
  ghat <- fhat
  inv_d <- ifelse(deg > 0, 1 / pmax(deg, 1), 0)
  r <- vapply(g$nbrs, function(nb) sum(inv_d[nb]), numeric(1))
  cov_ar <- mean(a * r) - mean(a) * mean(r)
  sum_d <- sum(deg)
  p_tilde <- sum(deg * a) / sum_d                                         # edge-end prevalence
  q_census <- sum(deg * D) / sum_d                                        # perceived edge-end prevalence (census)
  mis <- mean(fhat - f, na.rm = TRUE)
  mean_ghat <- mean(ghat, na.rm = TRUE)
  gap <- mean_ghat - p
  E <- g$edges
  deg_assort <- if (nrow(E) > 1) suppressWarnings(cor(c(deg[E[, 1]], deg[E[, 2]]), c(deg[E[, 2]], deg[E[, 1]]))) else NA_real_
  edge_homophily <- if (nrow(E) > 0) mean(a[E[, 1]] == a[E[, 2]]) else NA_real_
  # Pluralistic ignorance (definition): the agents holding the majority attitude
  # believe, on average, that their attitude is in the minority.
  maj <- if (p >= 0.5) 1L else 0L
  own_share <- if (maj == 1L) ghat else 1 - ghat          # perceived share of the majority attitude
  maj_mean <- mean(own_share[a == maj], na.rm = TRUE)
  share_maj_perceiving_minority <- mean(own_share[a == maj] < 0.5, na.rm = TRUE)
  list(
    n = n, n_edges = nrow(E), mean_degree = mean(deg), sd_degree = sd(deg), max_degree = max(deg),
    degree_assortativity = deg_assort, edge_homophily = edge_homophily,
    p = p, p_tilde = p_tilde, q_hat_census = q_census, mean_perceived_prevalence = mean_ghat,
    gap = gap, term_misperception = mis, term_structure = cov_ar,
    decomposition_residual = gap - (mis + cov_ar),
    falsified_share = mean(D != a),
    falsified_share_a1 = if (any(a == 1)) mean(D[a == 1] != 1) else NA_real_,
    falsified_share_a0 = if (any(a == 0)) mean(D[a == 0] != 0) else NA_real_,
    deg_weighted_net_falsification = q_census - p_tilde,
    majority_attitude = maj,
    majority_share = max(p, 1 - p),
    majority_mean_perceived_share = maj_mean,
    share_majority_perceiving_minority = share_maj_perceiving_minority,
    pluralistic_ignorance = isTRUE(maj_mean < 0.5),
    mean_alpha = mean(st$alpha),
    n_internalized = st$n_internalized %||% 0L,
    homophily_swaps = st$g$homophily_swaps %||% 0L,
    converged = isTRUE(st$converged), rounds = st$rounds
  )
}

# Terms in the majority-attitude frame (sign-flipped when the majority attitude
# is 0), so pluralistic ignorance is always the negative direction.
majority_frame <- function(gt) {
  s <- if (gt$p >= 0.5) 1 else -1
  list(structure = s * gt$term_structure, misperception = s * gt$term_misperception, gap = s * gt$gap)
}

# What a survey can record about each respondent (ego) and its neighbors.
SURVEY_OPTIONS <- c(
  degree            = "Respondent's number of contacts (degree)",
  declaration       = "Respondent's own public declaration",
  ego_perceptions   = "Perceived attitude of each contact (contacts anonymous)",
  neighbor_degrees  = "Each contact's number of contacts",
  linked            = "Each contact's identity and true attitude (linked design)"
)

# Uniform sample of m respondents without replacement. Perceptions are face
# value (perceived attitude of a contact = the contact's declaration);
# perceived prevalence = share of contacts declaring 1, plus optional
# elicitation noise (sd, clipped to [0, 1]).
survey <- function(st, m, survey_seed = 1L, noise_sd = 0) {
  set.seed(as.integer(survey_seed))
  n <- st$g$n; m <- min(as.integer(m), n)
  egos <- sample.int(n, m)
  recs <- lapply(egos, function(i) {
    nb <- st$g$nbrs[[i]]
    slot <- if (length(nb) > 1) sample.int(length(nb)) else seq_along(nb)   # shuffle: contacts anonymous
    nb <- nb[slot]
    fhat <- if (length(nb)) mean(st$D[nb]) else NA_real_
    ghat <- if (noise_sd > 0 && !is.na(fhat)) min(1, max(0, fhat + rnorm(1, 0, noise_sd))) else fhat
    list(id = i, a = st$a[i], b = st$D[i], d = length(nb), ghat = ghat, nb = nb)
  })
  respondents <- data.frame(
    respondent_id = vapply(recs, `[[`, integer(1), "id"),
    attitude = vapply(recs, `[[`, integer(1), "a"),
    perceived_prevalence = vapply(recs, `[[`, numeric(1), "ghat"),
    degree = vapply(recs, `[[`, integer(1), "d"),
    declaration = vapply(recs, `[[`, integer(1), "b"))
  ego_network <- do.call(rbind, lapply(recs, function(r) if (r$d > 0) data.frame(
    respondent_id = r$id, neighbor_slot = seq_len(r$d),
    perceived_neighbor_attitude = st$D[r$nb], neighbor_degree = st$g$deg[r$nb],
    neighbor_id = r$nb, neighbor_attitude = st$a[r$nb]) else NULL))
  list(egos = egos, m = m, survey_seed = survey_seed, noise_sd = noise_sd,
       respondents = respondents, ego_network = ego_network)
}

# Apply a survey design: keep only the columns the design records.
apply_design <- function(sv, include = names(SURVEY_OPTIONS)) {
  resp_cols <- c("respondent_id", "attitude", "perceived_prevalence",
                 if ("degree" %in% include) "degree", if ("declaration" %in% include) "declaration")
  out <- list(respondents = sv$respondents[, resp_cols, drop = FALSE])
  if ("ego_perceptions" %in% include) {
    ego_cols <- c("respondent_id", "neighbor_slot", "perceived_neighbor_attitude",
                  if ("neighbor_degrees" %in% include) "neighbor_degree",
                  if ("linked" %in% include) c("neighbor_id", "neighbor_attitude"))
    out$ego_network <- sv$ego_network[, ego_cols, drop = FALSE]
  }
  out
}

# Edge-end test: perceived edge-end prevalence (mean perceived attitude over
# all reported contacts) minus true edge-end prevalence estimated from the
# respondents' own attitudes weighted by their number of contacts. Needs only
# the anonymous ego-network design. In a census it equals the degree-weighted
# net falsification exactly, and is exactly zero without misperception.
edge_end_estimates <- function(respondents, ego_network) {
  d <- table(factor(ego_network$respondent_id, levels = respondents$respondent_id))
  d <- as.numeric(d)
  sum_d <- sum(d)
  q_hat <- sum(ego_network$perceived_neighbor_attitude) / sum_d
  p_tilde_hat <- sum(d * respondents$attitude) / sum_d
  list(q_hat = q_hat, p_tilde_hat = p_tilde_hat, net_misperception = q_hat - p_tilde_hat)
}

edge_end_bootstrap <- function(respondents, ego_network, B = 500L, seed = 1L) {
  set.seed(seed)
  ids <- respondents$respondent_id
  d <- as.numeric(table(factor(ego_network$respondent_id, levels = ids)))
  q <- as.numeric(tapply(ego_network$perceived_neighbor_attitude, factor(ego_network$respondent_id, levels = ids), sum))
  q[is.na(q)] <- 0
  a <- respondents$attitude
  stat <- function(idx) sum(q[idx]) / sum(d[idx]) - sum(d[idx] * a[idx]) / sum(d[idx])
  boots <- replicate(B, stat(sample.int(length(ids), replace = TRUE)))
  list(estimate = stat(seq_along(ids)), ci_lower = unname(quantile(boots, 0.025)), ci_upper = unname(quantile(boots, 0.975)), B = B)
}
