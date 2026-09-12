# Export of one run: respondents.csv, ego_network.csv (per the survey design),
# ground_truth.json, params.json, trajectory.csv, state.rds.

alpha_spec_string <- function(p) switch(p$alpha_dist,
  uniform = "uniform", beta = sprintf("beta(%g,%g)", p$alpha_a, p$alpha_b), point = sprintf("point(%g)", p$alpha_point))

parse_alpha_spec <- function(s) {
  s <- trimws(s)
  if (s == "uniform") return(list(alpha_dist = "uniform"))
  mb <- regmatches(s, regexec("^beta\\(\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*\\)$", s))[[1]]
  if (length(mb) == 3) return(list(alpha_dist = "beta", alpha_a = as.numeric(mb[2]), alpha_b = as.numeric(mb[3])))
  mp <- regmatches(s, regexec("^point\\(\\s*([0-9.]+)\\s*\\)$", s))[[1]]
  if (length(mp) == 2) return(list(alpha_dist = "point", alpha_point = as.numeric(mp[2])))
  stop("cannot parse alpha spec: ", s)
}

flat_params <- function(p) {
  keep <- c("scenario", "n", "topology", "m", "k", "mean_degree", "s", "k_out", "beta", "pi0",
            "init_decl", "lambda", "pure", "centrality", "psi", "homophily", "internalization", "max_rounds", "seed")
  out <- p[keep]; out$alpha <- alpha_spec_string(p); out$label <- scenario_label(p); out$scenario_name <- scenario_name(p)
  out
}

write_run <- function(st, sv, dir, include = names(SURVEY_OPTIONS), write_state = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ds <- apply_design(sv, include)
  write.csv(ds$respondents, file.path(dir, "respondents.csv"), row.names = FALSE)
  if (!is.null(ds$ego_network)) write.csv(ds$ego_network, file.path(dir, "ego_network.csv"), row.names = FALSE)
  gt <- ground_truth(st)
  jsonlite::write_json(gt, file.path(dir, "ground_truth.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)
  pj <- flat_params(st$params); pj$m <- sv$m; pj$survey_seed <- sv$survey_seed; pj$noise_sd <- sv$noise_sd
  pj$survey_records <- as.list(include)
  jsonlite::write_json(pj, file.path(dir, "params.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE, na = "null")
  if (!is.null(st$trajectory)) write.csv(st$trajectory, file.path(dir, "trajectory.csv"), row.names = FALSE)
  if (write_state) saveRDS(list(attitude = st$a, attitude_initial = st$a0, declaration = st$D, declaration_initial = st$D0,
                                alpha = st$alpha, edges = st$g$edges, params = st$params), file.path(dir, "state.rds"))
  invisible(gt)
}

run_record <- function(st, gt, extra = list()) {
  pj <- flat_params(st$params)
  pj$s <- pj$s %||% NA
  obs <- gt[c("n_edges", "mean_degree", "sd_degree", "max_degree")]
  names(obs) <- c("n_edges", "obs_mean_degree", "obs_sd_degree", "obs_max_degree")   # the params column mean_degree is the ER input
  c(pj, obs, gt[c("degree_assortativity", "edge_homophily", "p", "p_tilde", "q_hat_census", "mean_perceived_prevalence",
                  "gap", "term_misperception", "term_structure", "falsified_share", "deg_weighted_net_falsification",
                  "majority_mean_perceived_share", "share_majority_perceiving_minority", "pluralistic_ignorance",
                  "converged", "rounds")], extra)
}
