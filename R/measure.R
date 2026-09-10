# Measurement model (design-2026-09-09.md §3) and ground truth
# (identifiability note §5): survey extraction from a fixed point, tier files
# T0..T4, and the T3 edge-end estimator (Proposition 4).

# Population quantities and the three-term decomposition
#   E[ĝ] - p = extrapolation (0 here) + E[f̂ - f] (misperception) + Cov(a, r) (structure).
ground_truth <- function(st) {
  g <- st$g; n <- g$n; deg <- g$deg; a <- st$a; D <- st$D
  p <- mean(a)
  f    <- vapply(g$nbrs, function(nb) if (length(nb)) mean(a[nb]) else NA_real_, numeric(1))
  fhat <- vapply(g$nbrs, function(nb) if (length(nb)) mean(D[nb]) else NA_real_, numeric(1))
  ghat <- fhat
  inv_d <- ifelse(deg > 0, 1 / pmax(deg, 1), 0)
  r <- vapply(g$nbrs, function(nb) sum(inv_d[nb]), numeric(1))          # reach
  cov_ar <- mean(a * r) - mean(a) * mean(r)
  sum_d <- sum(deg)
  p_tilde <- sum(deg * a) / sum_d
  d_tilde <- sum(deg * D) / sum_d                                        # census q̂
  mis <- mean(fhat - f, na.rm = TRUE)
  mean_ghat <- mean(ghat, na.rm = TRUE)
  gap <- mean_ghat - p
  E <- g$edges
  deg_assort <- if (nrow(E) > 1) suppressWarnings(cor(c(deg[E[, 1]], deg[E[, 2]]), c(deg[E[, 2]], deg[E[, 1]]))) else NA_real_
  edge_homophily <- if (nrow(E) > 0) mean(a[E[, 1]] == a[E[, 2]]) else NA_real_
  maj <- if (p > 0.5) 1L else 0L
  pi_signature <- (p > 0.5 && mean_ghat < 0.5) || (p < 0.5 && mean_ghat > 0.5)
  pi_anchor <- if (p > 0.5) mean(ghat[a == 1] < 0.5, na.rm = TRUE) > 0.5 else if (p < 0.5) mean(ghat[a == 0] > 0.5, na.rm = TRUE) > 0.5 else FALSE
  list(
    n = n, n_edges = nrow(E), mean_degree = mean(deg), sd_degree = sd(deg), max_degree = max(deg),
    degree_assortativity = deg_assort, edge_homophily = edge_homophily,
    p = p, p_tilde = p_tilde, q_hat_census = d_tilde, mean_ghat = mean_ghat,
    gap = gap, term_misperception = mis, term_structure = cov_ar,
    decomposition_residual = gap - (mis + cov_ar),
    falsified_share = mean(D != a),
    falsified_share_a1 = if (any(a == 1)) mean(D[a == 1] != 1) else NA_real_,
    falsified_share_a0 = if (any(a == 0)) mean(D[a == 0] != 0) else NA_real_,
    deg_weighted_net_falsification = d_tilde - p_tilde,
    majority_attitude = maj,
    pi_signature = isTRUE(pi_signature), pi_anchor = isTRUE(pi_anchor),
    mean_alpha = mean(st$alpha), mean_credence = mean(st$c),
    n_internalized = st$n_internalized %||% 0L,
    homophily_swaps = st$g$homophily_swaps %||% 0L,
    converged = isTRUE(st$converged), rounds = st$rounds
  )
}

# Terms in the majority-attitude frame (sign-flipped when the majority attitude
# is 0), so pluralistic ignorance is always the negative direction.
majority_frame <- function(gt) {
  s <- if (gt$p > 0.5) 1 else -1
  list(structure = s * gt$term_structure, misperception = s * gt$term_misperception, gap = s * gt$gap)
}

# Uniform ego sample of size m without replacement. Perceptions are face-value
# (â_ij = D_j); ĝ_i = f̂_i plus optional elicitation noise (sd, clipped to [0,1]).
survey <- function(st, m, survey_seed = 1L, ghat_noise_sd = 0) {
  set.seed(as.integer(survey_seed))
  n <- st$g$n; m <- min(as.integer(m), n)
  egos <- sample.int(n, m)
  recs <- lapply(egos, function(i) {
    nb <- st$g$nbrs[[i]]
    slot <- if (length(nb) > 1) sample.int(length(nb)) else seq_along(nb)   # shuffle: no linkage below T4
    nb <- nb[slot]
    fhat <- if (length(nb)) mean(st$D[nb]) else NA_real_
    ghat <- if (ghat_noise_sd > 0 && !is.na(fhat)) min(1, max(0, fhat + rnorm(1, 0, ghat_noise_sd))) else fhat
    list(ego_id = i, a = st$a[i], b = st$D[i], d = length(nb), ghat = ghat,
         ahat = st$D[nb], alter_d = st$g$deg[nb], alter_id = nb, alter_a = st$a[nb])
  })
  ego_df <- data.frame(
    ego_id = vapply(recs, `[[`, integer(1), "ego_id"),
    a = vapply(recs, `[[`, integer(1), "a"),
    ghat = vapply(recs, `[[`, numeric(1), "ghat"),
    d = vapply(recs, `[[`, integer(1), "d"),
    b = vapply(recs, `[[`, integer(1), "b"))
  dy <- do.call(rbind, lapply(recs, function(r) if (r$d > 0) data.frame(
    ego_id = r$ego_id, alter_slot = seq_len(r$d), a = r$a, ghat = r$ghat, d = r$d, b = r$b,
    ahat = r$ahat, alter_d = r$alter_d, alter_id = r$alter_id, alter_a = r$alter_a) else NULL))
  tiers <- list(
    T0  = ego_df[, c("ego_id", "a", "ghat")],
    T1  = ego_df[, c("ego_id", "a", "ghat", "d")],
    T2  = ego_df[, c("ego_id", "a", "ghat", "d", "b")],
    T3  = dy[, c("ego_id", "alter_slot", "a", "ghat", "d", "b", "ahat")],
    T3p = dy[, c("ego_id", "alter_slot", "a", "ghat", "d", "b", "ahat", "alter_d")],
    T4  = dy
  )
  list(egos = egos, m = m, survey_seed = survey_seed, ghat_noise_sd = ghat_noise_sd, tiers = tiers)
}

# T3 estimator (Proposition 4): q̂ vs p̃ from the same unlinked ego sample.
edge_end_estimates <- function(T3) {
  egos <- T3[!duplicated(T3$ego_id), c("ego_id", "a", "d")]
  sum_d <- sum(egos$d)
  q_hat <- sum(T3$ahat) / sum_d
  p_tilde_hat <- sum(egos$d * egos$a) / sum_d
  list(q_hat = q_hat, p_tilde_hat = p_tilde_hat, net_misperception = q_hat - p_tilde_hat)
}

# Ego-level bootstrap CI for the edge-end test.
edge_end_bootstrap <- function(T3, B = 500L, seed = 1L) {
  set.seed(seed)
  ids <- unique(T3$ego_id); by_ego <- split(T3, T3$ego_id)
  stat <- function(sel) {
    d <- vapply(sel, function(id) by_ego[[as.character(id)]]$d[1], numeric(1))
    a <- vapply(sel, function(id) by_ego[[as.character(id)]]$a[1], numeric(1))
    q <- vapply(sel, function(id) sum(by_ego[[as.character(id)]]$ahat), numeric(1))
    sum(q) / sum(d) - sum(d * a) / sum(d)
  }
  boots <- replicate(B, stat(sample(ids, length(ids), replace = TRUE)))
  est <- stat(ids)
  list(estimate = est, ci_lower = unname(quantile(boots, 0.025)), ci_upper = unname(quantile(boots, 0.975)), B = B)
}
