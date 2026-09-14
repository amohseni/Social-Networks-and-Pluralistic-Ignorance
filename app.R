# Social Networks and Pluralistic Ignorance: data generator (Shiny GUI over R/).
# Run from the repo root:  Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
# Self-contained: everything it needs is in this folder (R/ is sourced below); all
# outputs are downloads, nothing is written outside a temporary directory.

library(shiny)
library(ggplot2)
library(DT)
library(igraph)

root <- normalizePath(".")
for (f in c("engine.R", "measure.R", "export.R", "corpus.R", "theme.R")) source(file.path(root, "R", f))
cache_dir <- file.path(tempdir(), "pi_cache"); dir.create(cache_dir, showWarnings = FALSE)

# zip a directory's contents (zip package if present, else the system zip)
make_zip <- function(zipfile, dir) {
  if (requireNamespace("zip", quietly = TRUE)) return(zip::zip(zipfile, files = list.files(dir, recursive = TRUE), root = dir, mode = "cherry-pick"))
  owd <- setwd(dir); on.exit(setwd(owd)); utils::zip(zipfile, files = list.files(".", recursive = TRUE), flags = "-q")
}

SCENARIO_CHOICES <- c("1. Conformity from a random start" = "S1", "2. Private change of mind" = "S3", "3. Well-connected minority (friendship paradox)" = "S2")

# ------------------------------------------------------------------ text

description_band <- fluidRow(
  style = PI_BAND_STYLE,
  column(width = 4, div(HTML(
    "<strong>The question:</strong> Survey data often show that most people hold an attitude yet believe it is rare. Two different processes produce this pattern: genuine pluralistic ignorance, and network structure with accurate perceptions. This tool generates blinded datasets from both, with a key, so that statistical methods for telling them apart can be tested.
    </br></br>
    <strong>The model:</strong> A population of \\(n\\) agents on an undirected network. Agent \\(i\\) has a private attitude \\(A_i \\in \\{0,1\\}\\), a public declaration \\(D_i \\in \\{0,1\\}\\), and a conformity parameter \\(c_i \\in [0,1]\\). Declaring \\(D\\) pays
    \\[U_i(D) = c_i \\, N_i(D) + (1-c_i) \\, \\mathbf{1}[D = A_i],\\]
    where \\(N_i(D)\\) is the share of \\(i\\)'s neighbors declaring \\(D\\). Agents best-respond one at a time in a random order each round until a full round passes with no change in declarations. Attitudes are fixed.
    </br></br>
    <strong>Open-Source Code:</strong> All the <a href='https://www.r-project.org/'>R</a> and <a href='https://shiny.rstudio.com/'>Shiny</a> code is available at <a href='https://github.com/amohseni/Social-Networks-and-Pluralistic-Ignorance'>www.github.com/amohseni</a>."))),
  column(width = 4, div(HTML(
    "<strong>Pluralistic ignorance:</strong> obtains just in case the agents holding the majority attitude mistakenly believe, on average, that their attitude is in the minority. Each agent's perceived prevalence \\(\\hat{g}_i\\) is the share of its neighbors declaring \\(1\\) (perceptions are at face value).
    </br></br>
    <strong>The alternative explanation:</strong> the friendship paradox, coupled with a positive correlation between centrality and the minority attitude. Your neighbors are better connected than you on average, so well-connected agents dominate everyone's local sample. If they lean toward the minority attitude, the majority sees a local majority for the other side, perceives accurately, and still concludes it is in the minority.
    </br></br>
    <strong>Two terms:</strong> with \\(p\\) the prevalence of attitude \\(1\\), \\(f_i\\) the true share of \\(i\\)'s neighbors holding it, \\(\\hat{f}_i\\) the share declaring it, and \\(r_j\\) the reach of \\(j\\) (the sum of \\(1/d_i\\) over its neighbors \\(i\\)),
    \\[\\mathbb{E}[\\hat{g}] - p = \\mathbb{E}[\\hat{f} - f] + \\mathrm{Cov}(A, r).\\]
    The first term is misperception (declarations differ from attitudes). The second is structure (who is seen how often); it is exactly zero on regular networks."))),
  column(width = 4, div(HTML(
    "<strong>Scenarios:</strong>
    <ol>
      <li><strong>Conformity from a random start.</strong> Attitudes and declarations start at random; conformity then reshapes the declarations. Sometimes, by chance, this produces pluralistic ignorance.</li>
      <li><strong>Private change of mind.</strong> Initially everyone holds and declares the same attitude. Then attitudes change so that the old attitude is in the minority, but the preference for conformity can produce pluralistic ignorance by keeping the old declarations in place.</li>
      <li><strong>Well-connected minority.</strong> More central agents are initially more likely to both hold and declare the minority attitude. This can produce the appearance of pluralistic ignorance when there is none: every agent perceives its neighbors' attitudes accurately.</li>
    </ol>
    <strong>Use:</strong> <em>Generate corpus</em> builds a blinded set of datasets that all show the pattern, from all three scenarios, with a key kept aside. Hand the blinded folder to the analysts; score their predictions in <em>Score predictions</em>. <em>Explore</em> runs single scenarios to see how the pattern arises.")))
)

guide_html <- HTML("
<h4>1. The model</h4>
<p>A population of \\(n\\) agents on an undirected network. Each agent \\(i\\) has a private attitude \\(A_i \\in \\{0,1\\}\\), a public declaration \\(D_i \\in \\{0,1\\}\\), and a conformity parameter \\(c_i \\in [0,1]\\) drawn from a chosen distribution. Declaring \\(D\\) pays \\(U_i(D) = c_i N_i(D) + (1-c_i)\\,\\mathbf{1}[D = A_i]\\), with \\(N_i(D)\\) the share of \\(i\\)'s neighbors currently declaring \\(D\\). So an agent declares its own attitude whenever \\((1-c_i) \\ge c_i\\,(1 - 2N_i(A_i))\\), and otherwise declares the other one; ties go to the private attitude. Agents update one at a time in a fresh random order each round; the run stops at the first full round with no change (a fixed point), or at the round cap (reported as not converged).</p>

<h4>2. Pluralistic ignorance</h4>
<p>Pluralistic ignorance obtains just in case the agents holding the majority attitude mistakenly believe, on average, that their attitude is in the minority. Formally, with attitude \\(1\\) the majority (\\(p > 1/2\\)) and \\(\\hat{g}_i\\) agent \\(i\\)'s perceived prevalence of attitude \\(1\\): the mean of \\(\\hat{g}_i\\) over agents with \\(A_i = 1\\) is below \\(1/2\\). Perceptions are at face value: \\(\\hat{g}_i\\) is the share of \\(i\\)'s neighbors declaring \\(1\\).</p>

<h4>3. The alternative explanation: the friendship paradox</h4>
<p>Suppose every perception is exactly right: everyone declares sincerely and reads declarations correctly. The pattern can still appear. Your neighbors have more connections than you do on average (the friendship paradox), so well-connected agents are over-represented in everyone's local sample. If those agents disproportionately hold the minority attitude, then most agents, majority holders included, see a neighborhood dominated by the minority attitude, and projecting from what they see, conclude that they are in the minority. The extreme case is a star: the hub holds attitude \\(0\\), every leaf holds \\(1\\), every leaf correctly sees only a \\(0\\), and the whole population but one believes attitude \\(1\\) is rare. Nobody is wrong about anyone they observe.</p>

<h4>4. Two terms, one square</h4>
<p>Let \\(p\\) be the prevalence of attitude \\(1\\), \\(f_i\\) the true share of \\(i\\)'s neighbors holding it, \\(\\hat{f}_i\\) the share declaring it, and \\(r_j = \\sum_{i \\in N(j)} 1/d_i\\) the reach of agent \\(j\\) (how much weight \\(j\\) carries in others' local samples; mean reach is exactly \\(1\\)). Then, exactly,
\\[\\mathbb{E}[\\hat{g}] - p = \\mathbb{E}[\\hat{f} - f] + \\mathrm{Cov}(A, r).\\]
The <em>misperception term</em> is nonzero only when declarations differ from attitudes: pluralistic ignorance in the usual sense. The <em>structure term</em> is nonzero only when attitude correlates with reach: the friendship-paradox contribution, identically zero on regular networks. In Explore, the decomposition bars show both terms for the current run in the majority attitude's frame (pluralistic ignorance is the negative direction), and the mechanism square plots the structure term against the misperception term over a cloud of runs from every scenario.</p>

<h4>5. The scenarios and what to look for</h4>
<ol>
<li><strong>Conformity from a random start.</strong> Attitudes and declarations start at random (attitudes with prevalence \\(\\pi_0\\)); conformity then reshapes the declarations. Sometimes, by chance, this produces pluralistic ignorance: the declarations lock the majority into voicing the minority view. More conformist populations (more mass of \\(c_i\\) near \\(1\\)) make that outcome more common.</li>
<li><strong>Private change of mind.</strong> Initially everyone holds and declares attitude \\(1\\). Then a fraction \\(\\psi > 1/2\\) privately changes to \\(0\\), so the old attitude is now in the minority while the declarations still say \\(1\\). Either the old declarations tip over, or the preference for conformity keeps them in place, producing pluralistic ignorance: the new majority keeps voicing the view it no longer holds and, hearing only that view, believes itself to be the minority.</li>
<li><strong>Well-connected minority.</strong> More central agents are initially more likely to both hold and declare the minority attitude: a share \\(1 - \\pi_0\\) of agents receives attitude \\(0\\), chosen with probability proportional to centrality to the power \\(\\lambda\\) (degree, eigenvector, or betweenness). With conformity off (\\(c_i = 0\\)) every agent perceives its neighbors' attitudes accurately, the misperception term is exactly \\(0\\), and yet the majority can come to believe it is the minority: the appearance of pluralistic ignorance when there is none. In Explore, conformity can also be turned on for this scenario, which mixes the two mechanisms with known shares; the blinded corpus uses the pure case.</li>
</ol>

<h4>6. The blinded corpus and the key</h4>
<p>The <em>Generate corpus</em> tab is the main tool. It produces a set of datasets that all show the pattern (runs whose population does not are discarded and counted), drawn from all three scenarios in the numbers you choose. Every hidden parameter is drawn at random per dataset from ranges you can adjust under advanced options: population size, links per agent, prevalence, the conformity distribution (\\(c_i \\sim \\mathrm{beta}(a, 1)\\)), the centrality exponent and measure. All scenarios use the same network family (preferential attachment) so that network shape alone is not a giveaway. To blind the data further, each dataset's attitude coding is flipped with probability \\(1/2\\) (so the majority is sometimes coded \\(0\\), sometimes \\(1\\)), respondent and contact identifiers are random, and dataset numbers are shuffled.</p>
<p>The downloads are <code>blinded</code>, to hand to the analysts: a <code>README.txt</code> describing the files and the task without revealing anything about individual datasets, and one folder per dataset with <code>respondents.csv</code> and, if contact perceptions are recorded, <code>ego_network.csv</code>. Kept aside, as a second download: <code>key.csv</code>, one row per dataset with the mechanism (genuine pluralistic ignorance vs friendship paradox), the scenario, every parameter, and the ground truth (both terms, the majority's true and perceived share, the falsifying share, the edge-end test on the sample); and the key package with the full fixed point of each run, so any other sample or survey design can be regenerated.</p>

<h4>7. Survey designs: what the data record</h4>
<p>Every dataset records each respondent's <em>attitude</em> and <em>perceived prevalence</em>. Each further box adds columns; what can be inferred grows with them:</p>
<table class='table table-condensed' style='max-width: 1000px;'>
<tr><th>Recorded</th><th>File and columns</th><th>What it allows</th></tr>
<tr><td>attitude, perceived prevalence</td><td><code>respondents.csv</code>: respondent_id, attitude, perceived_prevalence</td><td>the pattern itself; the two mechanisms are indistinguishable from this alone</td></tr>
<tr><td>+ number of contacts</td><td><code>respondents.csv</code> + degree</td><td>the attitude-degree covariance; structure alone can be refuted (never established) when degrees are too even to carry the gap</td></tr>
<tr><td>+ own declaration</td><td><code>respondents.csv</code> + declaration</td><td>the falsification channel directly: the share whose declaration differs from their attitude</td></tr>
<tr><td>+ perceived attitude of each contact, contacts anonymous</td><td><code>ego_network.csv</code>: respondent_id, neighbor_slot, perceived_neighbor_attitude</td><td>the edge-end test: mean perceived attitude over all reported contacts minus the degree-weighted mean of respondents' own attitudes equals the degree-weighted net misperception; exactly zero without misperception</td></tr>
<tr><td>+ each contact's number of contacts</td><td><code>ego_network.csv</code> + neighbor_degree</td><td>reach, hence both terms, without assuming neutral degree mixing</td></tr>
<tr><td>+ contacts' identities and true attitudes</td><td><code>ego_network.csv</code> + neighbor_id, neighbor_attitude</td><td>misperception per pair and per direction; the benchmark (not blinded in any useful sense)</td></tr>
</table>

<h4>8. Scoring predictions</h4>
<p>The analysts return a CSV with <code>dataset_id</code>, <code>prediction</code> (<code>genuine</code> or <code>structure</code>), and optionally <code>score</code> (their probability of genuine pluralistic ignorance). <em>Score predictions</em> reports accuracy, the confusion table, accuracy by scenario, and, when scores are given, the area under the ROC curve.</p>

<h4>9. Extensions (advanced options in Explore, off by default)</h4>
<ul>
<li><em>Homophily rewiring</em> \\(h\\): degree-preserving edge swaps that raise the share of same-attitude ties toward a target fraction \\(h\\) of edges.</li>
<li><em>Internalization</em>: each round, an agent declaring against its attitude adopts the declaration as its attitude with this probability.</li>
<li><em>Sincere initial declarations</em> for the random start; other network families (random, lattice, small world, core-periphery, complete, ring, star).</li>
</ul>
<p>Research sweeps over full parameter grids run from the command line (<code>Rscript R/sweep.R grids/corpus-v1.yaml corpora/v1</code>).</p>
")

# ------------------------------------------------------------------ UI

sl <- function(id, label, min, max, value, step) sliderInput(id, HTML(label), min = min, max = max, value = value, step = step, width = "100%")
h5s <- function(x) tags$h5(x, style = "margin-top: 22px; font-weight: 500; color: #444;")

explore_controls <- wellPanel(
  actionButton("run", "RUN", class = "btn-primary", width = "100%"),
  h5s("Scenario"),
  radioButtons("scenario", NULL, SCENARIO_CHOICES),
  conditionalPanel("input.scenario != 'S3'", sl("pi0", "Prevalence of attitude 1 \\(\\pi_0\\):", 0.5, 0.99, 0.6, 0.01)),
  conditionalPanel("input.scenario == 'S3'", sl("psi", "Fraction changing their mind \\(\\psi\\):", 0.5, 1, 0.7, 0.01)),
  conditionalPanel("input.scenario == 'S2'",
    checkboxInput("pure", HTML("No conformity (all \\(c_i = 0\\)): structure alone"), TRUE),
    selectInput("centrality", "Centrality measure:", c("degree", "eigenvector", "betweenness")),
    sl("lambda", "Centrality exponent \\(\\lambda\\) (0 = uncorrelated):", 0, 12, 4, 0.5)),
  conditionalPanel("!(input.scenario == 'S2' && input.pure)",
    h5s("Conformity"),
    selectInput("c_dist", HTML("Distribution of the conformity parameter \\(c_i\\):"), c("beta(a, b)" = "beta", "uniform on [0, 1]" = "uniform", "the same for everyone" = "point")),
    conditionalPanel("input.c_dist == 'beta'", fluidRow(column(6, numericInput("c_a", "a", 3, min = 0.01, step = 0.1)), column(6, numericInput("c_b", "b", 1, min = 0.01, step = 0.1)))),
    conditionalPanel("input.c_dist == 'point'", sl("c_point", "\\(c\\):", 0, 1, 0.5, 0.01))),
  h5s("Network"),
  selectInput("topology", "Network:", c("preferential attachment" = "ba", "random (Erdos-Renyi)" = "er", "regular lattice" = "regular",
                                        "small world" = "small_world", "core-periphery" = "core_periphery", "complete" = "complete", "ring" = "ring", "star" = "star")),
  sl("n", "Number of agents \\(n\\):", 10, 2000, 200, 10),
  conditionalPanel("input.topology == 'ba'", sl("m_ba", "Links per new agent \\(m\\):", 1, 10, 2, 1)),
  conditionalPanel("input.topology == 'er'", sl("mean_degree", "Mean number of contacts:", 1, 30, 6, 0.5)),
  conditionalPanel("input.topology == 'regular' || input.topology == 'small_world'", sl("k", "Contacts per agent \\(k\\) (even):", 2, 30, 4, 2)),
  conditionalPanel("input.topology == 'small_world'", sl("beta", "Rewiring probability \\(\\beta\\):", 0, 1, 0.1, 0.01)),
  conditionalPanel("input.topology == 'core_periphery'", sl("s_core", "Core size \\(s\\):", 2, 100, 10, 1), sl("k_out", "Periphery links into the core:", 1, 10, 1, 1)),
  h5s("Replication"),
  numericInput("seed", "Seed (replicate r uses seed + r - 1):", 1, min = 0, step = 1),
  sl("replicates", "Replicates:", 1, 100, 1, 1),
  checkboxInput("advanced", "Show advanced options", FALSE),
  conditionalPanel("input.advanced",
    conditionalPanel("input.scenario == 'S1'", radioButtons("init_decl", "Initial declarations:", c("random", "sincere"), inline = TRUE)),
    sl("homophily", "Homophily rewiring \\(h\\) (degree preserving; 0 = off):", 0, 1, 0, 0.05),
    sl("internalization", "Internalization rate (0 = attitudes fixed):", 0, 1, 0, 0.01),
    numericInput("max_rounds", "Round cap:", 500, min = 1, step = 1),
    numericInput("draw_max_n", "Draw the network up to n =", 300, min = 10, step = 10),
    numericInput("bg_n", "Mechanism-square cloud: n per run", 150, min = 30, step = 10),
    actionButton("bg_recompute", "Recompute cloud"))
)

explore_tab <- tabPanel("Explore", fluidRow(style = "padding-top: 20px;",
  column(4, explore_controls),
  column(8,
    tableOutput("summary"),
    tags$p(tags$b("Network"), " (fill = attitude, ring = declaration)"), plotOutput("net_plot", height = "600px"),
    tags$p(tags$b("Decomposition of the gap"), style = "margin-top: 30px;"), plotOutput("decomp_plot", height = "460px"),
    tags$p(tags$b("Mechanism square"), " over a cached cloud of runs from every scenario", style = "margin-top: 30px;"), plotOutput("square_plot", height = "540px"),
    tags$p(tags$b("Declarations over rounds"), style = "margin-top: 30px;"), plotOutput("traj_plot", height = "460px"),
    tags$p(tags$b("Ground truth per replicate"), style = "margin-top: 30px;"), DTOutput("rep_table"),
    downloadButton("download_run", "Download this run's data (all columns, plus ground truth)", style = "margin-top: 20px;"))
))

corpus_controls <- wellPanel(
  actionButton("gen", "GENERATE", class = "btn-primary", width = "100%"),
  h5s("Datasets per scenario"),
  fluidRow(column(4, numericInput("per_S1", "Random start", 30, min = 0, step = 1)),
           column(4, numericInput("per_S3", "Private change", 30, min = 0, step = 1)),
           column(4, numericInput("per_S2", "Well-connected minority", 30, min = 0, step = 1))),
  sl("respondents", "Respondents per dataset (capped at the population size):", 20, 1000, 150, 10),
  sl("n_range", "Population size \\(n\\) (drawn per dataset):", 50, 2000, c(200, 500), 10),
  checkboxGroupInput("records", "What the survey records (attitude and perceived prevalence always):",
                     choices = setNames(names(SURVEY_OPTIONS), SURVEY_OPTIONS), selected = c("degree", "declaration", "ego_perceptions", "neighbor_degrees")),
  numericInput("corpus_seed", "Seed:", 1, min = 0, step = 1),
  checkboxInput("corpus_advanced", "Show parameter ranges", FALSE),
  conditionalPanel("input.corpus_advanced",
    sl("prev_range", "Prevalence parameter (\\(\\pi_0\\), or \\(\\psi\\) for the private change):", 0.5, 0.99, c(0.55, 0.85), 0.01),
    sl("ca_S1", "Random start: conformity \\(c_i \\sim \\mathrm{beta}(a, 1)\\), range of \\(a\\):", 0.5, 12, c(3, 8), 0.5),
    sl("ca_S3", "Private change: range of \\(a\\):", 0.5, 12, c(2, 6), 0.5),
    sl("lambda_range", "Well-connected minority: centrality exponent \\(\\lambda\\):", 0, 12, c(2, 8), 0.5),
    checkboxGroupInput("cen_choices", "Centrality measures drawn from:", c("degree", "eigenvector", "betweenness"), selected = c("degree", "eigenvector", "betweenness"), inline = TRUE),
    checkboxGroupInput("m_choices", "Links per new agent drawn from:", 1:5, selected = 1:3, inline = TRUE))
)

corpus_tab <- tabPanel("Generate corpus", fluidRow(style = "padding-top: 20px;",
  column(4, corpus_controls),
  column(8,
    verbatimTextOutput("corpus_status"),
    fluidRow(column(4, downloadButton("download_blinded", "Blinded datasets (zip): for the analysts", style = "width: 100%")),
             column(4, downloadButton("download_key", "key.csv: keep", style = "width: 100%")),
             column(4, downloadButton("download_key_full", "Key package (zip): key plus full fixed points", style = "width: 100%"))),
    tags$p(tags$b("Key"), " (kept aside; one row per dataset)", style = "margin-top: 20px;"), DTOutput("key_table"),
    tags$p(tags$b("Where the datasets fall"), style = "margin-top: 30px;"), plotOutput("corpus_square", height = "520px"))
))

score_tab <- tabPanel("Score predictions", fluidRow(style = "padding-top: 20px;",
  column(4, wellPanel(
    fileInput("pred_file", "Predictions CSV (dataset_id, prediction, optional score):", accept = ".csv"),
    fileInput("key_file", "key.csv (leave empty to use the corpus generated in this session):", accept = ".csv"),
    actionButton("score", "SCORE", class = "btn-primary", width = "100%"))),
  column(8, verbatimTextOutput("score_text"), tableOutput("confusion"))
))

guide_tab <- tabPanel("Guide", fluidRow(style = "padding-top: 20px;", column(11, guide_html)))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 3, bootswatch = "paper"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;} pre.shiny-text-output {white-space: pre-wrap;} .table-summary td, .table-summary th {padding: 4px 14px;}"),
  tags$head(
    tags$script(HTML("window.MathJax = { tex: { inlineMath: [['\\\\(', '\\\\)']], displayMath: [['\\\\[', '\\\\]']] }, svg: { fontCache: 'global' } };")),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js", async = NA)),
  titlePanel("Social Networks and Pluralistic Ignorance"),
  description_band,
  tabsetPanel(type = "tabs", corpus_tab, explore_tab, score_tab, guide_tab)
)

# ------------------------------------------------------------------ server

params_from_input <- function(input) {
  p <- default_params()
  p$scenario <- input$scenario; p$pure <- isTRUE(input$pure)
  p$n <- as.integer(input$n); p$topology <- input$topology
  p$k <- as.integer(input$k); p$mean_degree <- input$mean_degree; p$m <- as.integer(input$m_ba)
  p$s <- if (input$topology == "core_periphery") as.integer(input$s_core) else NA
  p$k_out <- as.integer(input$k_out); p$beta <- input$beta
  p$pi0 <- input$pi0; p$init_decl <- input$init_decl
  p$c_dist <- input$c_dist; p$c_a <- input$c_a; p$c_b <- input$c_b; p$c_point <- input$c_point
  p$lambda <- input$lambda; p$centrality <- input$centrality; p$psi <- input$psi
  p$homophily <- input$homophily; p$internalization <- input$internalization
  p$max_rounds <- as.integer(input$max_rounds); p$seed <- as.integer(input$seed)
  p
}

background_cloud <- function(n_bg, progress = NULL) {
  f <- file.path(cache_dir, sprintf("bg_cloud_v3_n%d.rds", n_bg))
  if (file.exists(f)) return(readRDS(f))
  specs <- list()
  for (s in 1:25) specs[[length(specs) + 1]] <- list(scenario = "S1", n = n_bg, topology = "ba", m = 2, pi0 = 0.6, seed = s)
  for (s in 1:25) specs[[length(specs) + 1]] <- list(scenario = "S1", n = n_bg, topology = "ba", m = 2, pi0 = 0.6, c_dist = "beta", c_a = 6, c_b = 1, seed = 100 + s)
  for (lam in c(0, 1, 2, 4, 8)) for (s in 1:5) {
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = TRUE, seed = 2000 + 10 * lam + s)
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = FALSE, c_dist = "beta", c_a = 3, c_b = 1, seed = 6000 + 10 * lam + s)
  }
  for (psi in c(0.55, 0.65, 0.75, 0.85)) for (s in 1:6) specs[[length(specs) + 1]] <- list(scenario = "S3", n = n_bg, topology = "ba", m = 2, psi = psi, c_dist = "beta", c_a = 3, c_b = 1, seed = 9000 + round(100 * psi) + s)
  rows <- lapply(seq_along(specs), function(i) {
    if (!is.null(progress)) progress(i, length(specs))
    st <- run_scenario(specs[[i]]); gt <- ground_truth(st); mf <- majority_frame(gt)
    data.frame(class = scenario_label(st$params), structure = mf$structure, misperception = mf$misperception, pi = gt$pluralistic_ignorance)
  })
  out <- do.call(rbind, rows); saveRDS(out, f); out
}

square_plot <- function(bg, pts = NULL, pts_label = "this run") {
  g <- ggplot() +
    geom_hline(yintercept = 0, color = PI_COL$grey_light) + geom_vline(xintercept = 0, color = PI_COL$grey_light) +
    labs(x = "Structure term  Cov(A, reach)", y = "Misperception term  E[perceived - true local share]") + theme_pi() +
    theme(legend.text = element_text(size = 12))
  if (!is.null(bg)) {
    bg$class <- factor(bg$class, levels = names(CLASS_NAMES), labels = unname(CLASS_NAMES))
    g <- g + geom_point(data = bg, aes(structure, misperception, color = class), alpha = 0.45, size = 2.2) +
      scale_color_manual(values = setNames(unname(PI_CLASS_COL[names(CLASS_NAMES)]), unname(CLASS_NAMES)), drop = FALSE) +
      guides(color = guide_legend(ncol = 1))
  }
  if (!is.null(pts) && nrow(pts)) g <- g + geom_point(data = pts, aes(structure, misperception), shape = 21, size = 5, fill = PI_COL$orange, color = "white", stroke = 1.2) +
    annotate("text", x = pts$structure[1], y = pts$misperception[1], label = pts_label, hjust = -0.15, vjust = -0.7, size = 4.5)
  g
}

fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")

server <- function(input, output, session) {
  # ---- explore
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
      data.frame(scenario = scenario_name(x$st$params), seed = x$st$params$seed, prevalence = gt$p, majority_perceived_share = gt$majority_mean_perceived_share,
                 pluralistic_ignorance = gt$pluralistic_ignorance, gap = gt$gap, misperception = gt$term_misperception, structure = gt$term_structure,
                 mis_maj = mf$misperception, str_maj = mf$structure, falsified_share = gt$falsified_share,
                 edge_end_net_misperception = gt$deg_weighted_net_falsification, converged = gt$converged, rounds = gt$rounds,
                 mean_degree = gt$mean_degree, degree_assortativity = gt$degree_assortativity, edge_homophily = gt$edge_homophily, n_internalized = gt$n_internalized)
    }))
  })
  output$summary <- renderTable({
    gt <- first()$gt
    data.frame(check.names = FALSE,
      "Majority attitude" = as.character(gt$majority_attitude), "Its true share" = fmt(gt$majority_share),
      "What its holders perceive on average" = fmt(gt$majority_mean_perceived_share),
      "Pluralistic ignorance" = if (gt$pluralistic_ignorance) "yes" else "no",
      "Gap" = fmt(gt$gap), "Misperception term" = fmt(gt$term_misperception), "Structure term" = fmt(gt$term_structure),
      "Falsifying share" = fmt(gt$falsified_share),
      "Convergence" = sprintf("%s, %d rounds", if (gt$converged) "converged" else "NOT converged", gt$rounds))
  }, align = "c", spacing = "s", width = "100%", class = "table-summary")

  output$net_plot <- renderPlot({
    req(isTRUE(session$clientData$output_net_plot_width > 0), isTRUE(session$clientData$output_net_plot_height > 0))
    st <- first()$st; n <- st$g$n
    if (n > input$draw_max_n) {
      d <- data.frame(degree = st$g$deg, attitude = factor(st$a, levels = c(1, 0), labels = c("attitude 1", "attitude 0")))
      return(ggplot(d, aes(degree, fill = attitude)) + geom_histogram(binwidth = 1, position = "stack") +
               scale_fill_manual(values = c("attitude 1" = PI_COL$blue, "attitude 0" = PI_COL$orange)) +
               labs(x = "Number of contacts", y = "Agents", subtitle = sprintf("n = %d exceeds the drawing cap; degree histogram by attitude", n)) + theme_pi())
    }
    g <- graph_from_edgelist(st$g$edges, directed = FALSE)
    set.seed(1); lay <- layout_with_fr(g)
    par(mar = c(0, 0, 0, 0), family = "sans")
    col1 <- PI_COL$blue; col0 <- "#EE4000"
    plot(g, layout = lay, vertex.size = max(3, min(12, 180 / sqrt(n))), vertex.label = NA,
         vertex.color = ifelse(st$a == 1, col1, col0), vertex.frame.color = ifelse(st$D == 1, col1, col0),
         vertex.frame.width = 3, edge.color = "grey85", edge.width = 0.6)
    legend("topleft", bty = "n", pch = 21, pt.cex = 2.2, pt.bg = c(col1, col0, col1, col0), col = c(col1, col0, col0, col1), pt.lwd = 3, cex = 1.05, text.col = "#666666",
           legend = c("attitude 1, declares 1", "attitude 0, declares 0", "attitude 1, declares 0", "attitude 0, declares 1"))
  })
  output$decomp_plot <- renderPlot({
    req(isTRUE(session$clientData$output_decomp_plot_width > 0), isTRUE(session$clientData$output_decomp_plot_height > 0))
    gt <- first()$gt; mf <- majority_frame(gt)
    lev <- c("Gap", "Misperception", "Structure")
    d <- data.frame(term = factor(lev, levels = lev), value = c(mf$gap, mf$misperception, mf$structure))
    ggplot(d, aes(term, value, fill = term)) + geom_col(width = 0.6, show.legend = FALSE) + geom_hline(yintercept = 0, color = PI_COL$grey) +
      geom_text(aes(label = fmt(value), vjust = ifelse(value < 0, 1.5, -0.5)), size = 5, color = "#444444") +
      scale_fill_manual(values = c(PI_COL$black, PI_COL$orange, PI_COL$blue)) + scale_y_continuous(expand = expansion(mult = 0.2)) +
      labs(x = NULL, y = "Perceived minus true prevalence (majority frame)", subtitle = sprintf("Gap = misperception + structure (residual %.1e).", gt$decomposition_residual)) + theme_pi()
  })
  bg <- reactiveVal(NULL)
  observeEvent(input$bg_recompute, {
    f <- file.path(cache_dir, sprintf("bg_cloud_v3_n%d.rds", as.integer(input$bg_n))); if (file.exists(f)) file.remove(f)
    withProgress(message = "Computing background cloud", value = 0, { bg(background_cloud(as.integer(input$bg_n), function(i, t) setProgress(i / t))) })
  })
  output$square_plot <- renderPlot({
    req(isTRUE(session$clientData$output_square_plot_width > 0), isTRUE(session$clientData$output_square_plot_height > 0))
    req(runs())
    if (is.null(bg())) withProgress(message = "Computing background cloud (cached afterwards)", value = 0, { bg(background_cloud(as.integer(input$bg_n), function(i, t) setProgress(i / t))) })
    tb <- gt_table()
    square_plot(bg(), data.frame(structure = tb$str_maj, misperception = tb$mis_maj), pts_label = sprintf("this run (%d replicate%s)", nrow(tb), if (nrow(tb) > 1) "s" else ""))
  })
  output$traj_plot <- renderPlot({
    req(isTRUE(session$clientData$output_traj_plot_width > 0), isTRUE(session$clientData$output_traj_plot_height > 0))
    st <- first()$st; tr <- st$trajectory; req(tr)
    d <- rbind(data.frame(round = tr$round, value = tr$mean_declaration, series = "Declarations (share declaring 1)"),
               data.frame(round = tr$round, value = tr$mean_attitude, series = "Attitudes (share holding 1)"))
    d <- rbind(data.frame(round = 0, value = c(mean(st$D0), mean(st$a0)), series = unique(d$series)), d)
    outcome <- if (st$params$scenario == "S3") sprintf("Outcome: %s.", if (mean(st$D) < 0.5) "the old declarations tip over" else if (first()$gt$pluralistic_ignorance) "the old declarations persist (pluralistic ignorance)" else "partial") else "Declarations over rounds."
    ggplot(d, aes(round, value, color = series)) + geom_line(linewidth = 1.5) + geom_point(size = 2.5) +
      geom_hline(yintercept = 0.5, linetype = 2, color = PI_COL$grey) + scale_color_manual(values = c(PI_COL$blue, PI_COL$black)) + ylim(0, 1) +
      labs(x = "Round", y = "Share", subtitle = outcome) + theme_pi()
  })
  output$rep_table <- renderDT({
    tb <- gt_table()[, c("scenario", "seed", "prevalence", "majority_perceived_share", "pluralistic_ignorance", "gap", "misperception", "structure", "falsified_share", "edge_end_net_misperception", "converged", "rounds", "mean_degree", "degree_assortativity", "edge_homophily", "n_internalized")]
    datatable(tb, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("prevalence", "majority_perceived_share", "gap", "misperception", "structure", "falsified_share", "edge_end_net_misperception", "mean_degree", "degree_assortativity", "edge_homophily"), 3)
  })
  output$download_run <- downloadHandler(
    filename = function() sprintf("run_%s_n%d_seed%d.zip", input$scenario, input$n, input$seed),
    content = function(file) {
      tmp <- tempfile("run_"); dir.create(tmp)
      for (i in seq_along(runs())) { st <- runs()[[i]]$st; write_run(st, survey(st, st$g$n, 1L), file.path(tmp, sprintf("replicate_%03d", i))) }
      make_zip(file, tmp)
    }, contentType = "application/zip")

  # ---- generate corpus
  corpus <- reactiveVal(NULL)
  observeEvent(input$gen, {
    validate(need(sum(as.integer(input$per_S1), as.integer(input$per_S3), as.integer(input$per_S2)) > 0, "Ask for at least one dataset."))
    s <- default_corpus_settings()
    s$per_scenario <- c(S1 = as.integer(input$per_S1), S3 = as.integer(input$per_S3), S2 = as.integer(input$per_S2))
    s$respondents <- as.integer(input$respondents); s$records <- input$records; s$n_range <- as.integer(input$n_range)
    s$seed <- as.integer(input$corpus_seed); s$write_state <- TRUE
    s$prevalence_range <- input$prev_range; s$conformity_a_range <- list(S1 = input$ca_S1, S3 = input$ca_S3)
    s$lambda_range <- input$lambda_range; s$centrality <- input$cen_choices; s$ba_m <- as.integer(input$m_choices)
    out <- file.path(tempdir(), sprintf("blinded-%s-seed%d", format(Sys.time(), "%Y%m%d-%H%M%S"), s$seed)); dir.create(out, recursive = TRUE, showWarnings = FALSE)
    key <- withProgress(message = "Generating datasets", value = 0, {
      generate_blinded_corpus(s, out, progress = function(d, t, msg) setProgress(d / t, detail = msg))
    })
    corpus(list(key = key, out = out, attempts = attr(key, "attempts")))
  })
  output$corpus_status <- renderText({
    cp <- corpus()
    if (is.null(cp)) return("Choose the numbers and press GENERATE. Runs whose population does not show the pattern are discarded and counted as attempts.")
    k <- cp$key; tab <- table(k$scenario_code); cnt <- function(x) if (x %in% names(tab)) tab[[x]] else 0L
    sprintf("%d datasets generated. Download the blinded zip for the analysts (README.txt and datasets/dataset_XXXX/) and keep the key.\n\nAccepted (attempts): random start %d (%d); private change %d (%d); well-connected minority %d (%d).\nAttitude coding flipped in %d datasets. Pattern visible in every sample (%d of %d).",
            nrow(k), cnt("S1"), cp$attempts[["S1"]], cnt("S3"), cp$attempts[["S3"]], cnt("S2-pure"), cp$attempts[["S2"]],
            sum(k$attitude_coding_flipped), sum(k$pattern_in_sample), nrow(k))
  })
  output$key_table <- renderDT({
    cp <- corpus(); req(cp)
    datatable(cp$key, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("prevalence_parameter", "lambda", "majority_share", "majority_mean_perceived_share", "misperception_term", "structure_term", "falsified_share", "deg_weighted_net_falsification", "edge_end_test_in_sample_as_coded"), 3)
  })
  output$corpus_square <- renderPlot({
    req(isTRUE(session$clientData$output_corpus_square_width > 0), isTRUE(session$clientData$output_corpus_square_height > 0))
    cp <- corpus(); req(cp); k <- cp$key
    d <- data.frame(class = k$scenario_code, structure = k$structure_term, misperception = k$misperception_term)   # already in the majority frame
    square_plot(d) + labs(subtitle = "Every dataset shows the pattern; the mechanism differs")
  })
  output$download_blinded <- downloadHandler(
    filename = function() sprintf("%s_blinded.zip", basename(corpus()$out)),
    content = function(file) { cp <- corpus(); req(cp); make_zip(file, file.path(cp$out, "blinded")) },
    contentType = "application/zip")
  output$download_key_full <- downloadHandler(
    filename = function() sprintf("%s_key_package.zip", basename(corpus()$out)),
    content = function(file) { cp <- corpus(); req(cp); tmp <- tempfile("keypkg_"); dir.create(tmp)
      file.copy(file.path(cp$out, c("key.csv", "corpus.json")), tmp); file.copy(file.path(cp$out, "key_details"), tmp, recursive = TRUE)
      make_zip(file, tmp) },
    contentType = "application/zip")
  output$download_key <- downloadHandler(
    filename = function() sprintf("%s_key.csv", basename(corpus()$out)),
    content = function(file) { cp <- corpus(); req(cp); file.copy(file.path(cp$out, "key.csv"), file, overwrite = TRUE) })

  # ---- score
  scored <- eventReactive(input$score, {
    req(input$pred_file)
    key <- if (!is.null(input$key_file)) read.csv(input$key_file$datapath, stringsAsFactors = FALSE) else { validate(need(!is.null(corpus()), "Upload a key.csv or generate a corpus first.")); corpus()$key }
    score_predictions(key, read.csv(input$pred_file$datapath, stringsAsFactors = FALSE))
  })
  output$score_text <- renderText({
    r <- scored()
    sprintf("Scored %d datasets (%d in the key had no prediction).\nAccuracy: %.3f\nAccuracy by scenario: %s\nAUC (from scores): %s",
            r$n_scored, r$n_missing, r$accuracy, paste(sprintf("%s %.3f", names(r$accuracy_by_scenario), r$accuracy_by_scenario), collapse = "; "),
            if (is.na(r$auc)) "not available (no score column)" else fmt(r$auc))
  })
  output$confusion <- renderTable({ r <- scored(); as.data.frame.matrix(r$confusion) }, rownames = TRUE, caption = "Confusion table (rows: truth; columns: predicted)", caption.placement = "top")
}

shinyApp(ui, server)
