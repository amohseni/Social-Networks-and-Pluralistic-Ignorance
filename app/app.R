# PI / FP data generator: Shiny GUI over R/engine.R, R/measure.R, R/export.R, R/sweep.R.
# Run from the repo root:  Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(igraph)

root <- normalizePath("..")
for (f in c("engine.R", "measure.R", "export.R", "sweep.R")) source(file.path(root, "R", f))
cache_dir <- file.path(root, "app", "cache"); dir.create(cache_dir, showWarnings = FALSE)

CLASS_COLORS <- c("S1" = "#2a78d6", "S1 (PI)" = "#1f4f9c", "S2-pure" = "#1baf7a", "S2-mixed" = "#eda100", "S3" = "#e87ba4")
COL_A1 <- "#2a78d6"; COL_A0 <- "#eb6834"

theme_lab <- function() theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")

num <- function(id, label, value, min = NA, max = NA, step = NA, width = "100%") numericInput(id, label, value, min = min, max = max, step = step, width = width)

# ------------------------------------------------------------------ UI
sidebar_ui <- sidebar(
  width = 360, open = TRUE,
  actionButton("run", "Run", class = "btn-primary w-100"),
  accordion(
    open = c("Scenario", "Network"),
    accordion_panel("Scenario",
      radioButtons("scenario", NULL, c("S1: random attitudes and declarations" = "S1",
                                       "S2-pure: minority on central nodes, alpha = 1 (structure only)" = "S2-pure",
                                       "S2-mixed: minority on central nodes, alpha ~ F_alpha" = "S2-mixed",
                                       "S3: consensus, then a fraction psi flips attitude" = "S3")),
      conditionalPanel("input.scenario != 'S3'", sliderInput("pi0", "pi_0: prevalence of attitude x", 0.5, 0.99, 0.6, 0.01)),
      conditionalPanel("input.scenario == 'S1'", radioButtons("init_decl", "Initial declarations", c("random" = "random", "sincere" = "sincere"), inline = TRUE)),
      conditionalPanel("input.scenario == 'S2-pure' || input.scenario == 'S2-mixed'",
        sliderInput("lambda", "lambda: centrality exponent (0 = uncorrelated)", 0, 12, 4, 0.5),
        radioButtons("centrality", "Centrality measure", c("degree", "eigenvector"), inline = TRUE)),
      conditionalPanel("input.scenario == 'S3'", sliderInput("psi", "psi: fraction flipping to not-x", 0.5, 1, 0.7, 0.01))
    ),
    accordion_panel("Network",
      selectInput("topology", "Topology", c("Erdos-Renyi" = "er", "Barabasi-Albert" = "ba", "k-regular ring lattice" = "regular",
                                            "small world (Watts-Strogatz)" = "small_world", "core-periphery" = "core_periphery",
                                            "complete" = "complete", "ring" = "ring", "star" = "star")),
      num("n", "n: number of agents", 200, 5, 5000, 1),
      conditionalPanel("input.topology == 'er'", num("mean_degree", "mean degree (p_edge = mean degree / (n - 1))", 6, 1, NA, 0.5)),
      conditionalPanel("input.topology == 'ba'", num("m_ba", "m: edges per new node", 2, 1, 20, 1)),
      conditionalPanel("input.topology == 'regular' || input.topology == 'small_world'", num("k", "k: degree (even)", 4, 2, 50, 2)),
      conditionalPanel("input.topology == 'small_world'", sliderInput("beta", "beta: rewiring probability", 0, 1, 0.1, 0.01)),
      conditionalPanel("input.topology == 'core_periphery'", num("s_core", "s: core size", 10, 2, NA, 1), num("k_out", "k_out: periphery links into the core", 1, 1, 20, 1)),
      sliderInput("homophily", "Homophily rewiring h (extension; degree preserving)", 0, 1, 0, 0.05)
    ),
    accordion_panel("Types and credences",
      selectInput("alpha_dist", "F_alpha: truth-seeking weight", c("uniform on [0,1]" = "uniform", "beta(a, b)" = "beta", "point mass" = "point")),
      conditionalPanel("input.alpha_dist == 'beta'", layout_columns(num("alpha_a", "a", 1, 0.01, NA, 0.1), num("alpha_b", "b", 3, 0.01, NA, 0.1))),
      conditionalPanel("input.alpha_dist == 'point'", sliderInput("alpha_point", "alpha", 0, 1, 0.5, 0.01)),
      selectInput("credence", "Credence source", c("posterior from k paper-signals" = "signal", "uniform on (1/2, 1)" = "uniform", "point mass" = "point")),
      conditionalPanel("input.credence == 'signal'", num("credence_k", "k: number of signals", 1, 1, 20, 1)),
      conditionalPanel("input.credence == 'point'", sliderInput("credence_point", "c", 0.5, 1, 0.75, 0.01))
    ),
    accordion_panel("Dynamics",
      num("max_rounds", "Round cap", 500, 1, 10000, 1),
      sliderInput("internalization", "Internalization rate (extension; 0 = frozen beliefs)", 0, 1, 0, 0.01)
    ),
    accordion_panel("Seeds",
      num("seed", "Seed (replicate r uses seed + r - 1)", 1, 0, NA, 1),
      num("replicates", "Replicates", 1, 1, 200, 1)
    )
  )
)

run_tab <- nav_panel("Run view",
  layout_columns(fill = FALSE,
    value_box("p (prevalence of x)", textOutput("vb_p"), theme = "primary"),
    value_box("mean perceived prevalence", textOutput("vb_ghat"), theme = "secondary"),
    value_box("gap = misperception + structure", textOutput("vb_gap"), theme = "info"),
    value_box("PI signature", textOutput("vb_pi"), theme = "warning"),
    value_box("convergence", textOutput("vb_conv"), theme = "success")),
  layout_columns(
    card(card_header("Network: fill = attitude, ring = declaration"),
         num("draw_max_n", "Draw the network up to n =", 300, 10, 2000, 10, width = "50%"),
         plotOutput("net_plot", height = "440px")),
    card(card_header("Decomposition (majority-attitude frame; PI is negative)"), plotOutput("decomp_plot", height = "480px"))),
  layout_columns(
    card(card_header("Mechanism square"),
         layout_columns(num("bg_n", "background cloud: n per run", 150, 30, 1000, 10), actionButton("bg_recompute", "Recompute cloud")),
         plotOutput("square_plot", height = "420px")),
    card(card_header("Declaration trajectory"), plotOutput("traj_plot", height = "480px"))),
  card(card_header("Ground truth per replicate"), DTOutput("rep_table"))
)

export_tab <- nav_panel("Data export",
  layout_columns(col_widths = c(4, 8),
    card(card_header("Survey"),
      num("m", "m: ego sample size (capped at n)", 100, 1, NA, 1),
      num("survey_seed", "Survey seed", 1, 0, NA, 1),
      sliderInput("ghat_noise_sd", "Elicitation noise on ghat (sd; 0 = ghat equals fhat)", 0, 0.3, 0, 0.01),
      checkboxGroupInput("tiers", "Tiers to export", c("T0", "T1", "T2", "T3", "T3p", "T4"), selected = c("T0", "T1", "T2", "T3", "T3p", "T4")),
      checkboxInput("write_state", "Include state.rds (full fixed point; allows resampling any m later)", TRUE),
      downloadButton("download_zip", "Download zip (all replicates)", class = "btn-primary w-100")),
    card(card_header("Preview and edge-end test on the sample (replicate 1)"),
      selectInput("preview_tier", "Tier", c("T0", "T1", "T2", "T3", "T3p", "T4"), "T3", width = "30%"),
      verbatimTextOutput("edge_end_text"),
      DTOutput("preview_table")))
)

batch_tab <- nav_panel("Batch",
  layout_columns(col_widths = c(4, 8),
    card(card_header("Grid"),
      p("Comma-separated lists expand into a full factorial grid per scenario. Alpha and credence specs: ",
        code("uniform; beta(1,3); point(0.5)"), " and ", code("signal(k=1); uniform; point(0.75)"), "."),
      checkboxGroupInput("b_scenarios", "Scenarios", c("S1", "S2-pure", "S2-mixed", "S3"), selected = c("S1", "S2-pure", "S3"), inline = TRUE),
      textInput("b_n", "n", "200"),
      checkboxGroupInput("b_topos", "Topologies", c("er", "ba", "regular", "small_world", "core_periphery", "complete", "ring", "star"), selected = c("er", "ba", "regular"), inline = TRUE),
      textInput("b_mean_degree", "er: mean degree", "4, 8"), textInput("b_m", "ba: m", "2, 4"), textInput("b_k", "regular / small world: k", "4, 8"),
      textInput("b_beta", "small world: beta", "0.1"), textInput("b_s", "core-periphery: s", "10"), textInput("b_kout", "core-periphery: k_out", "1"),
      textInput("b_pi0", "pi_0 (S1, S2)", "0.6, 0.7, 0.8"), textInput("b_lambda", "lambda (S2)", "0, 2, 8"), textInput("b_psi", "psi (S3)", "0.6, 0.7, 0.8"),
      textInput("b_alpha", "alpha specs (S1, S2-mixed, S3)", "uniform; beta(1,3)"),
      textInput("b_credence", "credence specs", "signal(k=1)"),
      textInput("b_homophily", "homophily h", "0"), textInput("b_internalization", "internalization", "0"),
      num("b_seeds", "Seeds per cell", 5, 1, 1000, 1), num("b_seed_start", "First seed", 1, 0, NA, 1),
      textInput("b_m_list", "m list (census = all agents)", "50, census"),
      sliderInput("b_noise", "Elicitation noise sd", 0, 0.3, 0, 0.01),
      checkboxInput("b_state", "Write state.rds per run", TRUE),
      fileInput("b_yaml", "Or load a grid YAML (overrides the fields above)", accept = c(".yaml", ".yml")),
      textInput("b_out", "Output directory", file.path(root, "corpora", format(Sys.time(), "corpus-%Y%m%d-%H%M"))),
      actionButton("b_preview", "Preview grid", class = "w-100"), actionButton("b_run", "Run sweep", class = "btn-primary w-100")),
    card(card_header("Grid preview and results"),
      verbatimTextOutput("b_preview_text"),
      plotOutput("b_square", height = "380px"),
      DTOutput("b_manifest")))
)

schema_tab <- nav_panel("Schema and notes", card(
  h4("Vocabulary"),
  tags$ul(
    tags$li(strong("attitude"), " a_i in {0,1}: private; 1 = holds x. ", strong("declaration"), " D_i: public act; behavior b_i = D_i in the survey."),
    tags$li(strong("credence"), " c_i in (1/2, 1]: confidence in one's own attitude. ", strong("type"), " alpha_i in [0,1]: truth-seeking weight; payoff U_i(C) = alpha_i P_i(C) + (1 - alpha_i) N_i(C)."),
    tags$li(strong("misperception term"), " E[f̂ - f]: dyadic inaccuracy (pluralistic ignorance proper). ", strong("structure term"), " Cov(a, r), r = reach (friendship paradox)."),
    tags$li(strong("PI signature"), ": majority attitude's prevalence above 1/2 while mean perceived prevalence is below 1/2. ", strong("PI anchor"), ": over half of the majority-attitude holders perceive their attitude as the minority."),
    tags$li(strong("Tiers"), ": T0 (a, ghat); T1 + own degree; T2 + own behavior; T3 + per-alter perceptions, unlinked; T3p + alter degrees; T4 linked dyads.")),
  h4("Tier file schema (cumulative, self-contained CSVs)"),
  tags$table(class = "table table-sm",
    tags$tr(tags$th("File"), tags$th("Row unit"), tags$th("Columns")),
    tags$tr(tags$td("T0.csv"), tags$td("ego"), tags$td("ego_id, a, ghat")),
    tags$tr(tags$td("T1.csv"), tags$td("ego"), tags$td("+ d")),
    tags$tr(tags$td("T2.csv"), tags$td("ego"), tags$td("+ b")),
    tags$tr(tags$td("T3.csv"), tags$td("(ego, alter slot)"), tags$td("ego_id, alter_slot, a, ghat, d, b, ahat")),
    tags$tr(tags$td("T3p.csv"), tags$td("(ego, alter slot)"), tags$td("+ alter_d")),
    tags$tr(tags$td("T4.csv"), tags$td("(ego, alter slot)"), tags$td("+ alter_id, alter_a")),
    tags$tr(tags$td("ground_truth.json"), tags$td("run"), tags$td("population quantities, decomposition terms, PI flags, convergence")),
    tags$tr(tags$td("params.json"), tags$td("run"), tags$td("all generator, dynamics and survey parameters")),
    tags$tr(tags$td("state.rds"), tags$td("run"), tags$td("a, a0, D, c, alpha, edge list, params")),
    tags$tr(tags$td("manifest.csv"), tags$td("corpus"), tags$td("one row per run: parameters, label, ground-truth terms, paths"))),
  h4("Extensions beyond the 2026-09-09 design (all off by default)"),
  tags$ul(
    tags$li("small-world topology; homophily rewiring h (degree-preserving swaps toward same-attitude ties);"),
    tags$li("eigenvector-centrality weighting for S2 (design doc open question 6);"),
    tags$li("internalization rate (design doc open question 1): each round, an agent whose declaration differs from its attitude adopts the declaration with this probability, credence redrawn;"),
    tags$li("sincere initial declarations for S1; point-mass credence; elicitation noise on ghat (design doc open question 4)."))
))

ui <- page_navbar(title = "PI / FP data generator", theme = bs_theme(version = 5, preset = "flatly"),
                  header = tags$style(HTML("pre.shiny-text-output { white-space: pre-wrap; } .bslib-value-box .value-box-value { font-size: 1.35rem; }")),
                  sidebar = sidebar_ui, run_tab, export_tab, batch_tab, schema_tab)

# ------------------------------------------------------------------ server
params_from_input <- function(input) {
  p <- default_params()
  sc <- input$scenario
  p$scenario <- if (sc %in% c("S2-pure", "S2-mixed")) "S2" else sc
  p$pure <- sc == "S2-pure"
  p$n <- as.integer(input$n); p$topology <- input$topology
  p$k <- as.integer(input$k); p$mean_degree <- input$mean_degree; p$m <- as.integer(input$m_ba)
  p$s <- if (input$topology == "core_periphery") as.integer(input$s_core) else NA
  p$k_out <- as.integer(input$k_out); p$beta <- input$beta
  p$pi0 <- input$pi0; p$init_decl <- input$init_decl
  p$alpha_dist <- input$alpha_dist; p$alpha_a <- input$alpha_a; p$alpha_b <- input$alpha_b; p$alpha_point <- input$alpha_point
  p$credence <- input$credence; p$credence_k <- as.integer(input$credence_k); p$credence_point <- input$credence_point
  p$lambda <- input$lambda; p$centrality <- input$centrality; p$psi <- input$psi
  p$homophily <- input$homophily; p$internalization <- input$internalization
  p$max_rounds <- as.integer(input$max_rounds); p$seed <- as.integer(input$seed)
  p
}

background_cloud <- function(n_bg, progress = NULL) {
  f <- file.path(cache_dir, sprintf("bg_cloud_n%d.rds", n_bg))
  if (file.exists(f)) return(readRDS(f))
  specs <- list()
  for (s in 1:25) specs[[length(specs) + 1]] <- list(class = "S1", p = list(scenario = "S1", n = n_bg, topology = "er", mean_degree = 6, pi0 = 0.6, seed = s))
  for (s in 1:25) specs[[length(specs) + 1]] <- list(class = "S1", p = list(scenario = "S1", n = n_bg, topology = "er", mean_degree = 6, pi0 = 0.6, alpha_dist = "beta", alpha_a = 1, alpha_b = 3, seed = 100 + s))
  for (lam in c(0, 1, 2, 4, 8)) for (s in 1:5) {
    specs[[length(specs) + 1]] <- list(class = "S2-pure", p = list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = TRUE, seed = 2000 + 10 * lam + s))
    specs[[length(specs) + 1]] <- list(class = "S2-mixed", p = list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = FALSE, seed = 6000 + 10 * lam + s))
  }
  for (psi in c(0.55, 0.65, 0.75, 0.85)) for (s in 1:6) specs[[length(specs) + 1]] <- list(class = "S3", p = list(scenario = "S3", n = n_bg, topology = "regular", k = 8, psi = psi, alpha_dist = "beta", alpha_a = 1, alpha_b = 3, seed = 9000 + round(100 * psi) + s))
  rows <- lapply(seq_along(specs), function(i) {
    if (!is.null(progress)) progress(i, length(specs))
    gt <- ground_truth(run_scenario(specs[[i]]$p)); mf <- majority_frame(gt)
    data.frame(class = specs[[i]]$class, structure = mf$structure, misperception = mf$misperception, pi = gt$pi_signature)
  })
  out <- do.call(rbind, rows); saveRDS(out, f); out
}

square_plot <- function(bg, pts = NULL, pts_label = "this run") {
  g <- ggplot() +
    geom_hline(yintercept = 0, color = "grey70") + geom_vline(xintercept = 0, color = "grey70") +
    labs(x = "structure term  Cov(a, reach)", y = "misperception term  E[f̂ - f]", color = "class") + theme_lab()
  if (!is.null(bg)) g <- g + geom_point(data = bg, aes(structure, misperception, color = class), alpha = 0.35, size = 1.8) +
    scale_color_manual(values = CLASS_COLORS)
  if (!is.null(pts) && nrow(pts)) g <- g + geom_point(data = pts, aes(structure, misperception), shape = 21, size = 4, fill = "black", color = "white", stroke = 1) +
    annotate("text", x = pts$structure[1], y = pts$misperception[1], label = pts_label, hjust = -0.2, vjust = -0.6, size = 3.5)
  g
}

server <- function(input, output, session) {
  runs <- eventReactive(input$run, {
    p <- params_from_input(input); R <- max(1L, as.integer(input$replicates))
    withProgress(message = "Running replicates", value = 0, {
      lapply(seq_len(R), function(r) {
        pr <- p; pr$seed <- p$seed + r - 1L
        st <- run_scenario(pr); gt <- ground_truth(st)
        incProgress(1 / R)
        list(st = st, gt = gt)
      })
    })
  }, ignoreNULL = TRUE)

  first <- reactive({ req(runs()); runs()[[1]] })
  gt_table <- reactive({
    req(runs())
    do.call(rbind, lapply(runs(), function(x) {
      gt <- x$gt; mf <- majority_frame(gt)
      data.frame(label = scenario_label(x$st$params), seed = x$st$params$seed, p = gt$p, mean_ghat = gt$mean_ghat, gap = gt$gap,
                 misperception = gt$term_misperception, structure = gt$term_structure, mis_maj = mf$misperception, str_maj = mf$structure,
                 falsified_share = gt$falsified_share, dw_net_falsification = gt$deg_weighted_net_falsification,
                 pi_signature = gt$pi_signature, pi_anchor = gt$pi_anchor, converged = gt$converged, rounds = gt$rounds,
                 mean_degree = gt$mean_degree, sd_degree = gt$sd_degree, degree_assortativity = gt$degree_assortativity,
                 edge_homophily = gt$edge_homophily, n_internalized = gt$n_internalized)
    }))
  })

  fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")
  output$vb_p <- renderText({ gt <- first()$gt; sprintf("%s  (p̃ = %s)", fmt(gt$p), fmt(gt$p_tilde)) })
  output$vb_ghat <- renderText({ gt <- first()$gt; fmt(gt$mean_ghat) })
  output$vb_gap <- renderText({ gt <- first()$gt; sprintf("%s = %s + %s", fmt(gt$gap), fmt(gt$term_misperception), fmt(gt$term_structure)) })
  output$vb_pi <- renderText({ gt <- first()$gt; sprintf("%s (anchor: %s)", if (gt$pi_signature) "yes" else "no", if (gt$pi_anchor) "yes" else "no") })
  output$vb_conv <- renderText({ gt <- first()$gt; sprintf("%s in %d rounds", if (gt$converged) "converged" else "NOT converged", gt$rounds) })

  output$net_plot <- renderPlot({
    req(isTRUE(session$clientData$output_net_plot_width > 0), isTRUE(session$clientData$output_net_plot_height > 0))
    st <- first()$st; n <- st$g$n
    if (n > input$draw_max_n) {
      d <- data.frame(degree = st$g$deg, attitude = factor(st$a, levels = c(1, 0), labels = c("x", "not-x")))
      return(ggplot(d, aes(degree, fill = attitude)) + geom_histogram(binwidth = 1, position = "stack") +
               scale_fill_manual(values = c("x" = COL_A1, "not-x" = COL_A0)) +
               labs(title = sprintf("n = %d exceeds the drawing cap; degree histogram by attitude", n)) + theme_lab())
    }
    g <- graph_from_edgelist(st$g$edges, directed = FALSE)
    set.seed(1); lay <- layout_with_fr(g)
    par(mar = c(0, 0, 0, 0))
    plot(g, layout = lay, vertex.size = max(3, min(12, 180 / sqrt(n))), vertex.label = NA,
         vertex.color = ifelse(st$a == 1, COL_A1, COL_A0), vertex.frame.color = ifelse(st$D == 1, COL_A1, COL_A0),
         vertex.frame.width = 3, edge.color = "grey80", edge.width = 0.6)
    legend("bottomleft", bty = "n", pch = 21, pt.cex = 2, pt.bg = c(COL_A1, COL_A0, COL_A1, COL_A0), col = c(COL_A1, COL_A0, COL_A0, COL_A1), pt.lwd = 3,
           legend = c("attitude x, declares x", "attitude not-x, declares not-x", "attitude x, declares not-x (falsifying)", "attitude not-x, declares x (falsifying)"))
  })

  output$decomp_plot <- renderPlot({
    req(isTRUE(session$clientData$output_decomp_plot_width > 0), isTRUE(session$clientData$output_decomp_plot_height > 0))
    gt <- first()$gt; mf <- majority_frame(gt)
    d <- data.frame(term = factor(c("gap  E[ĝ] - p", "misperception  E[f̂ - f]", "structure  Cov(a, r)"), levels = c("gap  E[ĝ] - p", "misperception  E[f̂ - f]", "structure  Cov(a, r)")),
                    value = c(mf$gap, mf$misperception, mf$structure))
    ggplot(d, aes(term, value, fill = term)) + geom_col(width = 0.6, show.legend = FALSE) + geom_hline(yintercept = 0) +
      geom_text(aes(label = fmt(value), vjust = ifelse(value < 0, 1.4, -0.4))) +
      scale_fill_manual(values = c("grey40", CLASS_COLORS[["S3"]], CLASS_COLORS[["S2-pure"]])) +
      labs(x = NULL, y = "value (majority-attitude frame)", subtitle = sprintf("residual %.1e; falsified share %.3f; degree-weighted net falsification %.3f", gt$decomposition_residual, gt$falsified_share, gt$deg_weighted_net_falsification)) + theme_lab()
  })

  bg <- reactiveVal(NULL)
  observeEvent(input$bg_recompute, {
    f <- file.path(cache_dir, sprintf("bg_cloud_n%d.rds", as.integer(input$bg_n))); if (file.exists(f)) file.remove(f)
    withProgress(message = "Computing background cloud", value = 0, { bg(background_cloud(as.integer(input$bg_n), function(i, t) setProgress(i / t))) })
  })
  output$square_plot <- renderPlot({
    req(isTRUE(session$clientData$output_square_plot_width > 0), isTRUE(session$clientData$output_square_plot_height > 0))
    req(runs())
    if (is.null(bg())) withProgress(message = "Computing background cloud (cached afterwards)", value = 0, { bg(background_cloud(as.integer(input$bg_n), function(i, t) setProgress(i / t))) })
    tb <- gt_table()
    square_plot(bg(), data.frame(structure = tb$str_maj, misperception = tb$mis_maj), pts_label = sprintf("%s (%d replicate%s)", tb$label[1], nrow(tb), if (nrow(tb) > 1) "s" else ""))
  })

  output$traj_plot <- renderPlot({
    req(isTRUE(session$clientData$output_traj_plot_width > 0), isTRUE(session$clientData$output_traj_plot_height > 0))
    st <- first()$st; tr <- st$trajectory; req(tr)
    d <- rbind(data.frame(round = tr$round, value = tr$mean_declaration, series = "mean declaration (share declaring x)"),
               data.frame(round = tr$round, value = tr$mean_attitude, series = "mean attitude (share holding x)"))
    d <- rbind(data.frame(round = 0, value = c(mean(st$D0), mean(st$a0)), series = unique(d$series)), d)
    outcome <- if (st$params$scenario == "S3") sprintf("outcome: %s", if (first()$gt$pi_signature) "persistence (PI)" else if (mean(st$D) < 0.5) "tipping" else "partial") else ""
    ggplot(d, aes(round, value, color = series)) + geom_line() + geom_point(size = 1.5) + geom_hline(yintercept = 0.5, linetype = 2, color = "grey60") +
      scale_color_manual(values = c("black", COL_A1)) + ylim(0, 1) + labs(y = "share", subtitle = outcome, color = NULL) + theme_lab()
  })

  output$rep_table <- renderDT({
    tb <- gt_table()[, c("label", "seed", "p", "mean_ghat", "gap", "misperception", "structure", "falsified_share", "dw_net_falsification", "pi_signature", "pi_anchor", "converged", "rounds", "mean_degree", "sd_degree", "degree_assortativity", "edge_homophily", "n_internalized")]
    datatable(tb, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>% formatRound(c("p", "mean_ghat", "gap", "misperception", "structure", "falsified_share", "dw_net_falsification", "mean_degree", "sd_degree", "degree_assortativity", "edge_homophily"), 3)
  })

  # ---- export
  sv1 <- reactive({ st <- first()$st; survey(st, input$m, input$survey_seed, input$ghat_noise_sd) })
  output$preview_table <- renderDT({ datatable(sv1()$tiers[[input$preview_tier]], options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE) })
  output$edge_end_text <- renderText({
    sv <- sv1(); gt <- first()$gt; est <- edge_end_estimates(sv$tiers$T3); bb <- edge_end_bootstrap(sv$tiers$T3, B = 300)
    sprintf("Ego sample m = %d.  q̂ = %.4f,  p̃̂ = %.4f,  q̂ - p̃̂ = %.4f  [95%% bootstrap CI %.4f, %.4f]\nGround truth: degree-weighted net falsification = %.4f;  census q̂ = %.4f, p̃ = %.4f",
            sv$m, est$q_hat, est$p_tilde_hat, est$net_misperception, bb$ci_lower, bb$ci_upper, gt$deg_weighted_net_falsification, gt$q_hat_census, gt$p_tilde)
  })
  output$download_zip <- downloadHandler(
    filename = function() sprintf("pifp_%s_n%d_seed%d.zip", input$scenario, input$n, input$seed),
    content = function(file) {
      tmp <- tempfile("pifp_"); dir.create(tmp)
      for (i in seq_along(runs())) {
        st <- runs()[[i]]$st; sv <- survey(st, input$m, input$survey_seed + i - 1L, input$ghat_noise_sd)
        sv$tiers <- sv$tiers[input$tiers]
        write_run(st, sv, file.path(tmp, sprintf("run_%03d_seed%d", i, st$params$seed)), write_state = input$write_state)
      }
      owd <- setwd(tmp); on.exit(setwd(owd))
      zip(file, files = list.files(".", recursive = TRUE), flags = "-q")
    }, contentType = "application/zip")

  # ---- batch
  batch_spec <- reactive({
    if (!is.null(input$b_yaml)) return(yaml::read_yaml(input$b_yaml$datapath))
    split_specs <- function(s) { x <- trimws(unlist(strsplit(s, ";"))); x[nzchar(x)] }
    topos <- list()
    for (tp in input$b_topos) topos[[tp]] <- switch(tp,
      er = list(mean_degree = parse_num_list(input$b_mean_degree)), ba = list(m = parse_num_list(input$b_m)),
      regular = list(k = parse_num_list(input$b_k)), small_world = list(k = parse_num_list(input$b_k), beta = parse_num_list(input$b_beta)),
      core_periphery = list(s = parse_num_list(input$b_s), k_out = parse_num_list(input$b_kout)), list())
    blocks <- lapply(input$b_scenarios, function(sc) {
      b <- list(scenario = if (sc %in% c("S2-pure", "S2-mixed")) "S2" else sc, pure = sc == "S2-pure", n = parse_num_list(input$b_n), topologies = topos,
                credence = split_specs(input$b_credence), homophily = parse_num_list(input$b_homophily), internalization = parse_num_list(input$b_internalization))
      if (sc != "S3") b$pi0 <- parse_num_list(input$b_pi0)
      if (sc %in% c("S2-pure", "S2-mixed")) b$lambda <- parse_num_list(input$b_lambda)
      if (sc == "S3") b$psi <- parse_num_list(input$b_psi)
      if (sc != "S2-pure") b$alpha <- split_specs(input$b_alpha)
      b
    })
    list(name = basename(input$b_out), seeds_per_cell = as.integer(input$b_seeds), m = as.list(trimws(unlist(strsplit(input$b_m_list, ",")))), blocks = blocks)
  })
  preview_text <- reactiveVal("Press Preview grid.")
  observeEvent(input$b_preview, {
    spec <- batch_spec(); cells <- expand_grid_spec(spec); seeds <- spec$seeds_per_cell
    tab <- table(vapply(cells, scenario_label, character(1)))
    preview_text(sprintf("%d cells x %d seeds = %d runs (%s). Rough runtime %.1f min. Output: %s",
                         length(cells), seeds, length(cells) * seeds, paste(sprintf("%s: %d", names(tab), tab), collapse = ", "),
                         estimate_runtime_seconds(cells, seeds, length(spec[["m"]])) / 60, input$b_out))
  })
  manifest <- reactiveVal(NULL)
  observeEvent(input$b_run, {
    spec <- batch_spec(); cells <- expand_grid_spec(spec)
    withProgress(message = "Running sweep", value = 0, {
      mf <- run_sweep(cells, input$b_out, seeds_per_cell = spec[["seeds_per_cell"]], seed_start = as.integer(input$b_seed_start), m_list = spec[["m"]],
                      ghat_noise_sd = input$b_noise, write_state = input$b_state, progress = function(d, t) setProgress(d / t, detail = sprintf("%d / %d", d, t)), name = spec$name)
    })
    yaml::write_yaml(spec, file.path(input$b_out, "grid.yaml"))
    manifest(mf); preview_text(sprintf("Done: %d runs written to %s (manifest.csv, grid.yaml, corpus.json).", nrow(mf), input$b_out))
  })
  output$b_preview_text <- renderText(preview_text())
  output$b_manifest <- renderDT({ req(manifest()); datatable(manifest(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$b_square <- renderPlot({
    req(isTRUE(session$clientData$output_b_square_width > 0), isTRUE(session$clientData$output_b_square_height > 0))
    mf <- manifest(); req(mf)
    s <- ifelse(mf$p > 0.5, 1, -1)
    d <- data.frame(class = mf$label, structure = s * mf$term_structure, misperception = s * mf$term_misperception)
    square_plot(d) + labs(title = "Corpus coverage of the mechanism square")
  })
}

shinyApp(ui, server)
