# Export of one run: respondents.csv, ego_network.csv (per the survey design),
# ground_truth.json, params.json, trajectory.csv, state.rds.

conformity_spec_string <- function(p) switch(p$c_dist,
  uniform = "uniform", beta = sprintf("beta(%g,%g)", p$c_a, p$c_b), point = sprintf("point(%g)", p$c_point))

parse_conformity_spec <- function(s) {
  s <- trimws(s)
  if (s == "uniform") return(list(c_dist = "uniform"))
  mb <- regmatches(s, regexec("^beta\\(\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*\\)$", s))[[1]]
  if (length(mb) == 3) return(list(c_dist = "beta", c_a = as.numeric(mb[2]), c_b = as.numeric(mb[3])))
  mp <- regmatches(s, regexec("^point\\(\\s*([0-9.]+)\\s*\\)$", s))[[1]]
  if (length(mp) == 2) return(list(c_dist = "point", c_point = as.numeric(mp[2])))
  stop("cannot parse conformity spec: ", s)
}

flat_params <- function(p) {
  keep <- c("scenario", "n", "topology", "m", "k", "mean_degree", "s", "k_out", "beta", "pi0",
            "init_decl", "lambda", "pure", "centrality", "psi", "homophily", "internalization", "max_rounds", "seed")
  out <- p[keep]; out$conformity <- conformity_spec_string(p); out$label <- scenario_label(p); out$scenario_name <- scenario_name(p)
  out
}

# Export of one run. Writes the data configurations under <dir>/for_analysts/<C>/
# (the same layout as a corpus, with one dataset) and the ground truth under
# <dir>/key/: ground_truth.json, params.json, nodes.csv, edges.csv, trajectory.csv, state.rds.
write_run <- function(st, sv, dir, configs = CONFIG_CODES, add_pn = FALSE, write_state = TRUE,
                      include = NULL, dataset_id = "dataset_0001") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(include)) {                          # legacy per-column design: flat files in dir
    ds <- apply_design(sv, include)
    write.csv(ds$respondents, file.path(dir, "respondents.csv"), row.names = FALSE)
    if (!is.null(ds$ego_network)) write.csv(ds$ego_network, file.path(dir, "ego_network.csv"), row.names = FALSE)
  } else {
    write_config_files(file.path(dir, "for_analysts"), list(list(sv = sv, id = dataset_id)), configs, add_pn, sv$noise_sd, sv$alter_response_rate)
  }
  kdir <- if (is.null(include)) file.path(dir, "key") else dir
  dir.create(kdir, recursive = TRUE, showWarnings = FALSE)
  gt <- ground_truth(st)
  jsonlite::write_json(gt[setdiff(names(gt), c("pi_individual", "apparent_individual"))], file.path(kdir, "ground_truth.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)
  pj <- flat_params(st$params); pj$m <- sv$m; pj$survey_seed <- sv$survey_seed; pj$noise_sd <- sv$noise_sd; pj$alter_response_rate <- sv$alter_response_rate
  pj$configurations <- as.list(if (is.null(include)) configs else include); pj$add_pn <- add_pn
  jsonlite::write_json(pj, file.path(kdir, "params.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE, na = "null")
  if (is.null(include)) {
    r <- sv$respondents
    nodes <- data.frame(node_id = r$respondent_id, attitude = r$attitude, declaration = r$declaration,
                        perceived_prevalence = r$perceived_prevalence, perceived_contacts_mean = r$perceived_contacts_mean,
                        degree = r$degree, conformity = st$conformity, sampled = r$sampled, responded = r$responded,
                        believes_own_attitude_is_local_minority = gt$apparent_individual, pluralistic_ignorance = gt$pi_individual)
    write.csv(nodes, file.path(kdir, "nodes.csv"), row.names = FALSE)
    write.csv(data.frame(node_a = sv$edges[, 1], node_b = sv$edges[, 2]), file.path(kdir, "edges.csv"), row.names = FALSE)
    writeLines(key_readme(), file.path(kdir, "README.txt"))
  }
  if (!is.null(st$trajectory)) write.csv(st$trajectory, file.path(kdir, "trajectory.csv"), row.names = FALSE)
  if (write_state) saveRDS(list(attitude = st$Y, attitude_initial = st$Y0, declaration = st$X, declaration_initial = st$X0,
                                conformity = st$conformity, edges = st$g$edges, params = st$params), file.path(kdir, "state.rds"))
  invisible(gt)
}

# Zip a directory (relative paths inside the archive). Uses the zip package if
# installed, else the zip binary through utils::zip.
make_zip <- function(zipfile, dir) {
  old <- setwd(dir); on.exit(setwd(old))
  files <- list.files(".", recursive = TRUE, all.files = FALSE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(zipfile, files, mode = "mirror")
  else utils::zip(zipfile, files, flags = "-r9Xq")
  invisible(zipfile)
}

run_record <- function(st, gt, extra = list()) {
  pj <- flat_params(st$params)
  pj$s <- pj$s %||% NA
  obs <- gt[c("n_edges", "mean_degree", "sd_degree", "max_degree")]
  names(obs) <- c("n_edges", "obs_mean_degree", "obs_sd_degree", "obs_max_degree")   # the params column mean_degree is the ER input
  c(pj, obs, gt[c("degree_assortativity", "edge_homophily", "mu_Y", "mu_X", "mu_Y_edge", "mu_PN_edge", "mu_P",
                  "Delta_G", "perception_error", "exposure_bias", "perception_error_gross", "Delta_F", "falsified_share", "eta",
                  "apparent_share", "pattern_present", "pi_prevalence", "pi_prevalence_a1", "pi_prevalence_a0",
                  "converged", "rounds")], extra)
}
