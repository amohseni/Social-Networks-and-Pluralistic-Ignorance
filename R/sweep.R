# Batch sweeps: grid spec -> corpus directory with one folder per run and a
# manifest.csv. Callable from the Shiny app and from the command line:
#   Rscript R/sweep.R grids/corpus-v1.yaml corpora/v1 [--seeds N] [--n N] [--no-state]

parse_num_list <- function(s) {
  s <- trimws(unlist(strsplit(as.character(s), "[,;]")))
  s <- s[nzchar(s)]
  as.numeric(s)
}

# Expand one grid block into a list of parameter lists (one per cell).
expand_block <- function(block) {
  base <- default_params()
  topos <- block$topologies
  if (is.null(topos)) topos <- list(er = list())
  cells <- list()
  for (tp in names(topos)) {
    tparams <- topos[[tp]]; if (is.null(tparams) || length(tparams) == 0) tparams <- list(.dummy = 1)
    tgrid <- expand.grid(tparams, stringsAsFactors = FALSE)
    scalars <- setdiff(names(block), c("topologies", "alpha", "pi0", "lambda", "psi", "homophily", "internalization", "n"))
    lists <- list(
      alpha = block$alpha %||% "uniform",
      pi0 = block$pi0 %||% base$pi0,
      lambda = block$lambda %||% base$lambda,
      psi = block$psi %||% base$psi,
      homophily = block$homophily %||% 0,
      internalization = block$internalization %||% 0,
      n = block[["n"]] %||% base$n)
    ogrid <- expand.grid(lists, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(tgrid))) for (j in seq_len(nrow(ogrid))) {
      p <- base
      p$topology <- tp
      for (nm in setdiff(names(tgrid), ".dummy")) p[[nm]] <- tgrid[i, nm]
      for (nm in scalars) p[[nm]] <- block[[nm]]
      p[c("pi0", "lambda", "psi", "homophily", "internalization", "n")] <- ogrid[j, c("pi0", "lambda", "psi", "homophily", "internalization", "n")]
      p <- modifyList(p, parse_alpha_spec(ogrid$alpha[j]))
      cells[[length(cells) + 1]] <- p
    }
  }
  cells
}

expand_grid_spec <- function(spec) {
  cells <- unlist(lapply(spec$blocks, expand_block), recursive = FALSE)
  if (!is.null(spec[["n"]])) cells <- unlist(lapply(spec[["n"]], function(nn) lapply(cells, function(p) { p$n <- nn; p })), recursive = FALSE)
  cells
}

estimate_runtime_seconds <- function(cells, seeds_per_cell, n_m = 2L) {
  # Calibrated 2026-09-10 on an Apple Silicon Mac: 21 runs at n = 500 with two
  # m values took 3.8 s (about 0.18 s per run), engine and export included.
  per_run <- vapply(cells, function(p) 0.00015 * p$n + 0.05 * n_m, numeric(1))
  sum(per_run) * seeds_per_cell
}

run_sweep <- function(cells, out_dir, seeds_per_cell = 10L, seed_start = 1L, m_list = c("census"),
                      noise_sd = 0, include = names(SURVEY_OPTIONS), write_state = TRUE, progress = NULL, name = "corpus") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- list(); total <- length(cells) * seeds_per_cell; done <- 0L; t0 <- Sys.time()
  for (ci in seq_along(cells)) {
    for (sj in seq_len(seeds_per_cell)) {
      p <- cells[[ci]]; p$seed <- seed_start + (ci - 1L) * seeds_per_cell + (sj - 1L)
      st <- run_scenario(p); gt <- ground_truth(st)
      run_id <- sprintf("run_%06d", done + 1L)
      rdir <- file.path(out_dir, run_id)
      for (mm in m_list) {
        m_val <- if (identical(mm, "census") || is.na(suppressWarnings(as.numeric(mm)))) st$g$n else as.integer(mm)
        sv <- survey(st, m_val, survey_seed = p$seed + 10^6, noise_sd = noise_sd)
        mdir <- if (length(m_list) == 1) rdir else file.path(rdir, paste0("m_", if (m_val == st$g$n) "census" else m_val))
        write_run(st, sv, mdir, include = include, write_state = write_state && (mm == m_list[[1]]))
      }
      rows[[length(rows) + 1]] <- run_record(st, gt, list(run_id = run_id, cell = ci, path = rdir))
      done <- done + 1L
      if (!is.null(progress)) progress(done, total)
    }
  }
  manifest <- do.call(rbind, lapply(rows, function(r) as.data.frame(lapply(r, function(x) if (is.null(x) || length(x) == 0) NA else x), stringsAsFactors = FALSE)))
  write.csv(manifest, file.path(out_dir, "manifest.csv"), row.names = FALSE)
  jsonlite::write_json(list(name = name, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_cells = length(cells),
                            seeds_per_cell = seeds_per_cell, m_list = as.list(m_list), noise_sd = noise_sd, survey_records = as.list(include),
                            elapsed_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))),
                       file.path(out_dir, "corpus.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(manifest)
}

if (sys.nframe() == 0L && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) stop("usage: Rscript R/sweep.R <grid.yaml> <out_dir> [--seeds N] [--n N] [--no-state]")
  here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))))
  source(file.path(here, "engine.R")); source(file.path(here, "measure.R")); source(file.path(here, "export.R"))
  spec <- yaml::read_yaml(args[1]); cells <- expand_grid_spec(spec)
  seeds <- spec[["seeds_per_cell"]] %||% 10L
  if ("--seeds" %in% args) seeds <- as.integer(args[which(args == "--seeds") + 1])
  if ("--n" %in% args) cells <- lapply(cells, function(p) { p$n <- as.integer(args[which(args == "--n") + 1]); p })
  m_list <- spec[["m"]] %||% list("census")
  cat(sprintf("%d cells x %d seeds = %d runs; estimated %.0f s\n", length(cells), seeds, length(cells) * seeds,
              estimate_runtime_seconds(cells, seeds, length(m_list))))
  run_sweep(cells, args[2], seeds_per_cell = seeds, m_list = m_list, write_state = !("--no-state" %in% args),
            progress = function(d, t) if (d %% 50 == 0 || d == t) cat(sprintf("  %d / %d\n", d, t)), name = spec$name %||% basename(args[2]))
  cat("done:", args[2], "\n")
}
