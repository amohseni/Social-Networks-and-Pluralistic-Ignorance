# Truth-and-Conformity generative models for the PI/FP identifiability project.
# R port of for-claude/models/pi-fp/tc_pi_engine.py (2026-09-09), per
# design-2026-09-09.md. Base R only; no Shiny, no igraph. Deterministic given
# params$seed (set.seed is called inside run_scenario).
#
# Conventions: attitude a_i in {0,1}, 1 = holds x. Declaration D_i in {0,1}.
# Credence c_i in (1/2, 1] is confidence in one's own attitude. Type alpha_i in
# [0,1] is the truth-seeking weight. Payoff for declaring C (the paper's):
#   U_i(C) = alpha_i * P_i(C) + (1 - alpha_i) * N_i(C).
# Best response: declare a_i iff alpha_i (2 c_i - 1) >= (1 - alpha_i)(1 - 2 N_i(a_i)).
#
# Additions beyond the Python engine, each off by default and labeled as an
# extension in the GUI: small_world topology, degree-preserving homophily
# rewiring, eigenvector-centrality weighting for S2, internalization rate,
# sincere initial declarations for S1, point credence.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ------------------------------------------------------------------ parameters

default_params <- function() {
  list(
    scenario = "S1",            # S1 | S2 | S3 (S2 with pure = TRUE is S2-pure)
    n = 200L,
    topology = "er",            # complete | ring | regular | er | ba | star | core_periphery | small_world
    k = 4L,                     # regular, small_world: even degree
    mean_degree = 6,            # er: p_edge = mean_degree / (n - 1)
    m = 2L,                     # ba: edges per new node
    s = NA,                     # core_periphery: core size (default n %/% 20, at least 2)
    k_out = 1L,                 # core_periphery: periphery links into the core
    beta = 0.1,                 # small_world: rewiring probability
    pi0 = 0.6,                  # S1, S2: attitude-1 prevalence
    alpha_dist = "uniform",     # uniform | beta | point
    alpha_a = 1, alpha_b = 3,   # beta parameters
    alpha_point = 0.5,          # point type
    credence = "signal",        # signal | uniform | point
    credence_k = 1L,            # signal: number of signals
    credence_point = 0.75,      # point credence
    init_decl = "random",       # S1: random | sincere
    lambda = 4,                 # S2: centrality exponent
    pure = FALSE,               # S2: alpha = 1 for all (structure-only corner)
    centrality = "degree",      # S2: degree | eigenvector
    psi = 0.7,                  # S3: flipped fraction
    homophily = 0,              # extension: target share of edges rewired toward same-attitude ties
    internalization = 0,        # extension: per-round probability that a falsifying agent adopts its declaration
    max_rounds = 500L,
    seed = 1L
  )
}

merge_params <- function(p) {
  d <- default_params()
  for (nm in names(p)) d[[nm]] <- p[[nm]]
  d
}

# ------------------------------------------------------------------ topologies

canon_edges <- function(E, n) {
  if (is.null(E) || nrow(E) == 0) return(matrix(integer(0), 0, 2))
  E <- cbind(pmin(E[, 1], E[, 2]), pmax(E[, 1], E[, 2]))
  E <- E[E[, 1] != E[, 2], , drop = FALSE]
  E <- unique(E)
  storage.mode(E) <- "integer"
  E
}

ba_edges <- function(n, m) {
  m <- max(1L, min(as.integer(m), n - 1L))
  urn <- seq_len(m)                              # seed nodes; degree-proportional urn
  out <- vector("list", n)
  if (n > m) for (v in (m + 1L):n) {
    chosen <- integer(0)
    while (length(chosen) < m) {
      cand <- urn[sample.int(length(urn), 1L)]
      if (!(cand %in% chosen)) chosen <- c(chosen, cand)
    }
    out[[v]] <- cbind(chosen, v)
    urn <- c(urn, as.vector(rbind(chosen, v)))
  }
  do.call(rbind, out)
}

regular_edges <- function(n, k) {
  k <- max(2L, 2L * (as.integer(k) %/% 2L))
  k <- min(k, if (n %% 2 == 0) n - 2L else n - 1L)
  k <- max(2L, k)
  do.call(rbind, lapply(seq_len(k %/% 2), function(step) cbind(seq_len(n), ((seq_len(n) - 1L + step) %% n) + 1L)))
}

make_graph <- function(kind, n, k = 4L, mean_degree = 6, m = 2L, s = NA, k_out = 1L, beta = 0.1, p_edge = NA) {
  n <- as.integer(n)
  E <- switch(kind,
    complete = if (n >= 2) t(combn(n, 2)) else matrix(integer(0), 0, 2),
    ring = cbind(seq_len(n), c(seq_len(n)[-1], 1L)),
    regular = regular_edges(n, k),
    er = {
      pe <- if (is.na(p_edge)) mean_degree / (n - 1) else p_edge
      iu <- which(upper.tri(matrix(TRUE, n, n)), arr.ind = TRUE)
      iu[runif(nrow(iu)) < pe, , drop = FALSE]
    },
    ba = ba_edges(n, m),
    star = cbind(1L, seq_len(n)[-1]),
    core_periphery = {
      s <- as.integer(s %||% max(2L, n %/% 20L)); s <- max(2L, min(s, n))
      core <- if (s >= 2) t(combn(s, 2)) else matrix(integer(0), 0, 2)
      per <- if (n > s) do.call(rbind, lapply((s + 1L):n, function(v)
        cbind(sample.int(s, min(as.integer(k_out), s)), v))) else NULL
      rbind(core, per)
    },
    small_world = {
      E0 <- regular_edges(n, k)
      rew <- runif(nrow(E0)) < beta
      for (idx in which(rew)) {
        u <- E0[idx, 1]; w <- sample.int(n, 1L)
        while (w == u) w <- sample.int(n, 1L)
        E0[idx, 2] <- w
      }
      E0
    },
    stop("unknown topology: ", kind)
  )
  E <- canon_edges(E, n)
  # Enforce d_i >= 1 (the identifiability note assumes it): attach any isolate
  # to one uniformly random other node.
  touched <- logical(n); touched[c(E[, 1], E[, 2])] <- TRUE
  if (n >= 2) for (v in which(!touched)) {
    u <- sample.int(n - 1L, 1L); u <- if (u < v) u else u + 1L
    E <- rbind(E, c(min(u, v), max(u, v)))
    touched[c(u, v)] <- TRUE
  }
  build_graph(canon_edges(E, n), n)
}

build_graph <- function(E, n) {
  nb <- split(c(E[, 2], E[, 1]), factor(c(E[, 1], E[, 2]), levels = seq_len(n)))
  nb <- lapply(nb, function(x) sort(as.integer(x)))
  names(nb) <- NULL
  list(n = n, edges = E, nbrs = nb, deg = lengths(nb))
}

# Degree-preserving double-edge swaps that raise the share of same-attitude
# ties. h = target fraction of edges rewired; attempts capped at 20 |E|.
rewire_homophily <- function(g, a, h) {
  if (is.na(h) || h <= 0 || nrow(g$edges) < 2) return(g)
  E <- g$edges; ne <- nrow(E)
  keyf <- function(u, v) paste(pmin(u, v), pmax(u, v))
  eset <- new.env(hash = TRUE, size = 2L * ne)
  for (i in seq_len(ne)) assign(keyf(E[i, 1], E[i, 2]), TRUE, envir = eset)
  target <- round(h * ne); accepted <- 0L; attempts <- 0L
  while (accepted < target && attempts < 20L * ne) {
    attempts <- attempts + 1L
    ij <- sample.int(ne, 2L); i <- ij[1]; j <- ij[2]
    u1 <- E[i, 1]; v1 <- E[i, 2]; u2 <- E[j, 1]; v2 <- E[j, 2]
    cur <- (a[u1] == a[v1]) + (a[u2] == a[v2])
    cands <- list(c(u1, v2, u2, v1), c(u1, u2, v1, v2))
    for (cd in cands) {
      p1 <- cd[1:2]; p2 <- cd[3:4]
      if (p1[1] == p1[2] || p2[1] == p2[2]) next
      new <- (a[p1[1]] == a[p1[2]]) + (a[p2[1]] == a[p2[2]])
      if (new <= cur) next
      k1 <- keyf(p1[1], p1[2]); k2 <- keyf(p2[1], p2[2])
      if (k1 == k2 || exists(k1, envir = eset, inherits = FALSE) || exists(k2, envir = eset, inherits = FALSE)) next
      rm(list = c(keyf(u1, v1), keyf(u2, v2)), envir = eset)
      assign(k1, TRUE, envir = eset); assign(k2, TRUE, envir = eset)
      E[i, ] <- c(min(p1), max(p1)); E[j, ] <- c(min(p2), max(p2))
      accepted <- accepted + 1L
      break
    }
  }
  g <- build_graph(canon_edges(E, g$n), g$n)
  g$homophily_swaps <- accepted
  g
}

eigenvector_centrality <- function(g, iters = 500L) {
  x <- rep(1, g$n)
  for (it in seq_len(iters)) {
    y <- vapply(g$nbrs, function(nb) sum(x[nb]), numeric(1)) + 1e-12
    y <- y / sqrt(sum(y^2))
    if (max(abs(y - x)) < 1e-10) { x <- y; break }
    x <- y
  }
  x
}

# ------------------------------------------------------------------ draws

draw_credence <- function(n, p) {
  if (n == 0) return(numeric(0))
  switch(p$credence,
    uniform = runif(n, 0.5, 1),
    point = rep(min(1, max(0.5 + 1e-12, p$credence_point)), n),
    signal = {
      # k paper-signals from the believed state (pdf 2s on [0,1]); fold so
      # credence stays in (1/2, 1]; multiply likelihood ratios for k > 1.
      k <- max(1L, as.integer(p$credence_k))
      s <- matrix(sqrt(runif(n * k)), n, k)
      lr <- apply(s / (1 - s), 1, prod)
      post <- lr / (1 + lr)
      pmin(1, pmax(pmax(post, 1 - post), 0.5 + 1e-12))
    },
    stop("unknown credence source: ", p$credence))
}

draw_alpha <- function(n, p) {
  switch(p$alpha_dist,
    uniform = runif(n),
    beta = rbeta(n, p$alpha_a, p$alpha_b),
    point = rep(min(1, max(0, p$alpha_point)), n),
    stop("unknown alpha distribution: ", p$alpha_dist))
}

weighted_sample_wor <- function(w, k) {
  # Efraimidis-Spirakis keys in log form (numerically safe for large weights)
  keys <- log(runif(length(w))) / pmax(w, 1e-300)
  order(keys, decreasing = TRUE)[seq_len(k)]
}

# ------------------------------------------------------------------ dynamics

best_response <- function(i, a, c, alpha, D, nbrs) {
  nb <- nbrs[[i]]
  if (length(nb) == 0) return(a[i])                        # isolated: sincere
  n_own <- mean(D[nb] == a[i])
  gain <- alpha[i] * (2 * c[i] - 1) - (1 - alpha[i]) * (1 - 2 * n_own)
  if (gain > 0) return(a[i])
  if (gain < 0) return(1L - a[i])
  sample(0:1, 1L)                                          # tie: random (paper rule)
}

run_to_fixed_point <- function(st, p, log_trajectory = FALSE) {
  n <- st$g$n; nbrs <- st$g$nbrs
  a <- st$a; c <- st$c; alpha <- st$alpha; D <- st$D
  rho <- p$internalization %||% 0
  traj_D <- numeric(0); traj_a <- numeric(0); n_flips <- 0L
  converged <- FALSE; rounds <- 0L
  for (t in seq_len(p$max_rounds)) {
    ord <- sample.int(n); changed <- 0L
    for (i in ord) {
      d_new <- best_response(i, a, c, alpha, D, nbrs)
      if (d_new != D[i]) { D[i] <- d_new; changed <- changed + 1L }
    }
    flipped <- 0L
    if (rho > 0) {
      cand <- which(D != a)
      flip <- cand[runif(length(cand)) < rho]
      if (length(flip)) {
        a[flip] <- D[flip]; c[flip] <- draw_credence(length(flip), p)
        flipped <- length(flip); n_flips <- n_flips + flipped
      }
    }
    rounds <- t
    if (log_trajectory) { traj_D <- c(traj_D, mean(D)); traj_a <- c(traj_a, mean(a)) }
    # Absorbing state: a quiet round; with internalization on, additionally
    # no agent is left declaring against its attitude (else a later flip is possible).
    if (changed == 0L && flipped == 0L && (rho <= 0 || !any(D != a))) { converged <- TRUE; break }
  }
  st$a <- a; st$c <- c; st$D <- D
  st$rounds <- rounds; st$converged <- converged; st$n_internalized <- n_flips
  st$trajectory <- if (log_trajectory) data.frame(round = seq_along(traj_D), mean_declaration = traj_D, mean_attitude = traj_a) else NULL
  st
}

# ------------------------------------------------------------------ scenarios

new_state <- function(g, a, c, alpha, D, p) {
  list(g = g, a = as.integer(a), a0 = as.integer(a), c = c, alpha = alpha, D = as.integer(D), D0 = as.integer(D), params = p)
}

graph_from_params <- function(p) {
  make_graph(p$topology, p$n, k = p$k, mean_degree = p$mean_degree, m = p$m,
             s = p$s, k_out = p$k_out, beta = p$beta)
}

run_scenario <- function(params) {
  p <- merge_params(params)
  p$n <- as.integer(p$n)
  set.seed(as.integer(p$seed))
  g <- graph_from_params(p)
  n <- p$n
  if (p$scenario == "S1") {
    a <- as.integer(runif(n) < p$pi0)
    g <- rewire_homophily(g, a, p$homophily)
    D <- if (p$init_decl == "sincere") a else sample(0:1, n, replace = TRUE)
    st <- new_state(g, a, draw_credence(n, p), draw_alpha(n, p), D, p)
    st <- run_to_fixed_point(st, p, log_trajectory = TRUE)
  } else if (p$scenario == "S2") {
    cen <- if (p$centrality == "eigenvector") eigenvector_centrality(g) else as.numeric(g$deg)
    n_min <- floor((1 - p$pi0) * n)
    w <- pmax(cen, 1e-9)^p$lambda
    a <- rep(1L, n)
    if (n_min > 0) a[weighted_sample_wor(w, n_min)] <- 0L   # central nodes hold the minority attitude
    g <- rewire_homophily(g, a, p$homophily)
    alpha <- if (isTRUE(p$pure)) rep(1, n) else draw_alpha(n, p)
    st <- new_state(g, a, draw_credence(n, p), alpha, a, p)   # declarations start sincere
    st <- run_to_fixed_point(st, p, log_trajectory = TRUE)
  } else if (p$scenario == "S3") {
    a <- rep(1L, n); D <- rep(1L, n)                          # consensus on x: a fixed point
    cred <- draw_credence(n, p); alpha <- draw_alpha(n, p)
    flip <- sample.int(n, round(p$psi * n))
    a[flip] <- 0L; cred[flip] <- draw_credence(length(flip), p)
    g <- rewire_homophily(g, a, p$homophily)
    st <- new_state(g, a, cred, alpha, D, p)
    st <- run_to_fixed_point(st, p, log_trajectory = TRUE)
  } else stop("unknown scenario: ", p$scenario)
  st$params <- p
  st
}

scenario_label <- function(p) {
  if (p$scenario == "S2") if (isTRUE(p$pure)) "S2-pure" else "S2-mixed" else p$scenario
}
