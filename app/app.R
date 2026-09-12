# Social Networks and Pluralistic Ignorance: data generator (Shiny GUI over R/).
# Run from the repo root:  Rscript -e 'shiny::runApp("app", launch.browser = TRUE)'

library(shiny)
library(ggplot2)
library(DT)
library(igraph)

root <- normalizePath("..")
for (f in c("engine.R", "measure.R", "export.R", "sweep.R")) source(file.path(root, "R", f))
source(file.path(root, "app", "theme.R"))
cache_dir <- file.path(root, "app", "cache"); dir.create(cache_dir, showWarnings = FALSE)

# ------------------------------------------------------------------ text

description_band <- fluidRow(
  style = PI_BAND_STYLE,
  column(width = 4, div(HTML(
    "<strong>The question:</strong> Survey data often show that most people hold an attitude yet believe it is rare. Two different processes produce this pattern. This tool generates labeled data from each, and from their mixtures, so that methods for telling them apart can be tested.
    </br></br>
    <strong>The model:</strong> A population of \\(n\\) agents on an undirected network. Agent \\(i\\) has a private attitude \\(A_i \\in \\{0,1\\}\\), a public declaration \\(D_i \\in \\{0,1\\}\\), and a conformity parameter \\(\\alpha_i \\in [0,1]\\). Declaring \\(D\\) pays
    \\[U_i(D) = \\alpha_i \\, N_i(D) + (1-\\alpha_i) \\, \\mathbf{1}[D = A_i],\\]
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
    <ul>
      <li><strong>Conformity from a random start.</strong> Attitudes and declarations start at random; conformity then reshapes the declarations. Shows whether pluralistic ignorance can arise on its own.</li>
      <li><strong>Well-connected minority.</strong> The minority attitude is placed on central agents. With conformity off, every perception is exactly right and any gap is pure structure: the friendship-paradox case. Turning conformity on mixes the two mechanisms with known shares.</li>
      <li><strong>Private change of mind.</strong> Everyone held and declared attitude \\(1\\); then a fraction \\(\\psi > 1/2\\) privately changes to \\(0\\). Conformity can hold the old declarations in place: the classic preference-falsification case.</li>
    </ul>
    <strong>Use:</strong> pick a scenario and press RUN. The bars split the gap into its two terms; the mechanism square places the run against the other scenarios. Export data under a chosen survey design, or sweep a grid in Batch. Details in the Guide tab.")))
)

guide_html <- HTML("
<h4>1. The model</h4>
<p>A population of \\(n\\) agents on an undirected network. Each agent \\(i\\) has a private attitude \\(A_i \\in \\{0,1\\}\\), a public declaration \\(D_i \\in \\{0,1\\}\\), and a conformity parameter \\(\\alpha_i \\in [0,1]\\) drawn from a chosen distribution \\(F_\\alpha\\). Declaring \\(D\\) pays \\(U_i(D) = \\alpha_i N_i(D) + (1-\\alpha_i)\\,\\mathbf{1}[D = A_i]\\), with \\(N_i(D)\\) the share of \\(i\\)'s neighbors currently declaring \\(D\\). So an agent declares its own attitude whenever \\((1-\\alpha_i) \\ge \\alpha_i\\,(1 - 2N_i(A_i))\\), and otherwise declares the other one; ties go to the private attitude. Agents update one at a time in a fresh random order each round; the run stops at the first full round with no change (a fixed point), or at the round cap (reported as not converged).</p>

<h4>2. Pluralistic ignorance</h4>
<p>Pluralistic ignorance obtains just in case the agents holding the majority attitude mistakenly believe, on average, that their attitude is in the minority. Formally, with attitude \\(1\\) the majority (\\(p > 1/2\\)) and \\(\\hat{g}_i\\) agent \\(i\\)'s perceived prevalence of attitude \\(1\\): the mean of \\(\\hat{g}_i\\) over agents with \\(A_i = 1\\) is below \\(1/2\\). Perceptions are at face value: \\(\\hat{g}_i\\) is the share of \\(i\\)'s neighbors declaring \\(1\\). The summary row reports the majority attitude's true share, what its holders perceive on average, and the verdict.</p>

<h4>3. The alternative explanation: the friendship paradox</h4>
<p>Suppose every perception is exactly right: everyone declares sincerely and reads declarations correctly. The pattern can still appear. Your neighbors have more connections than you do on average (the friendship paradox), so well-connected agents are over-represented in everyone's local sample. If those agents disproportionately hold the minority attitude, then most agents, majority holders included, see a neighborhood dominated by the minority attitude, and projecting from what they see, conclude that they are in the minority. The extreme case is a star: the hub holds attitude \\(0\\), every leaf holds \\(1\\), every leaf correctly sees only a \\(0\\), and the whole population but one believes attitude \\(1\\) is rare. Nobody is wrong about anyone they observe.</p>

<h4>4. Two terms, one square</h4>
<p>Let \\(p\\) be the prevalence of attitude \\(1\\), \\(f_i\\) the true share of \\(i\\)'s neighbors holding it, \\(\\hat{f}_i\\) the share declaring it, and \\(r_j = \\sum_{i \\in N(j)} 1/d_i\\) the reach of agent \\(j\\) (how much weight \\(j\\) carries in others' local samples; mean reach is exactly \\(1\\)). Then, exactly,
\\[\\mathbb{E}[\\hat{g}] - p = \\mathbb{E}[\\hat{f} - f] + \\mathrm{Cov}(A, r).\\]
The <em>misperception term</em> \\(\\mathbb{E}[\\hat{f} - f]\\) is nonzero only when declarations differ from attitudes: this is pluralistic ignorance in the usual sense. The <em>structure term</em> \\(\\mathrm{Cov}(A, r)\\) is nonzero only when attitude correlates with reach: this is the friendship-paradox contribution, and it is identically zero on regular networks. The decomposition bars show both terms for the current run, in the majority attitude's frame so that pluralistic ignorance is always the negative direction. The mechanism square plots the structure term (horizontal) against the misperception term (vertical) over a cloud of runs from every scenario: the well-connected minority without conformity lies on the horizontal axis, the private change of mind on regular networks lies on the vertical axis, and mixtures fill the interior.</p>

<h4>5. The scenarios and what to look for</h4>
<ul>
<li><strong>Conformity from a random start.</strong> Attitudes are drawn independently with prevalence \\(\\pi_0\\); declarations start at random (or sincere). Conformity then pulls declarations toward local majorities. Run many replicates: some settle with the majority attitude openly declared (no pluralistic ignorance), others lock the majority into declaring the minority view. More conformist populations (more mass of \\(\\alpha\\) near \\(1\\)) make the second outcome more common.</li>
<li><strong>Well-connected minority.</strong> A share \\(1 - \\pi_0\\) of agents receives attitude \\(0\\), chosen with probability proportional to centrality to the power \\(\\lambda\\) (degree, eigenvector, or betweenness centrality). With conformity off (\\(\\alpha_i = 0\\)), declarations are sincere, the misperception term is exactly \\(0\\), and any gap is structure: raise \\(\\lambda\\) on a preferential-attachment network and watch the majority come to believe it is the minority without anyone misperceiving anyone. With conformity on, the conformity pressure of minority-dominated neighborhoods adds falsification on top of the structural effect, with both terms recorded.</li>
<li><strong>Private change of mind.</strong> Everyone holds and declares attitude \\(1\\). Then a fraction \\(\\psi > 1/2\\) privately switches to \\(0\\), so attitude \\(0\\) is now the majority, while declarations still say \\(1\\). Each switcher declares \\(0\\) only if its conformity is low enough given what its neighbors are still saying. Either the old declarations tip over (the trajectory falls below \\(1/2\\)) or they persist: the new majority keeps declaring the view it no longer holds and, seeing only such declarations, believes itself to be the minority. On regular networks the structure term is zero, so persistence is pure misperception.</li>
</ul>

<h4>6. Survey designs and the exported data</h4>
<p>An export is a uniform sample of \\(m\\) respondents. Every design records each respondent's <em>attitude</em> and <em>perceived prevalence</em>. Each further box in the export tab adds columns; what can be inferred grows with them:</p>
<table class='table table-condensed' style='max-width: 1000px;'>
<tr><th>Recorded</th><th>File and columns</th><th>What it allows</th></tr>
<tr><td>attitude, perceived prevalence</td><td><code>respondents.csv</code>: respondent_id, attitude, perceived_prevalence</td><td>the pattern itself; the two mechanisms are indistinguishable from this alone</td></tr>
<tr><td>+ number of contacts</td><td><code>respondents.csv</code> + degree</td><td>estimates the attitude-degree covariance; structure alone can be refuted (never established) when the degrees are too even to carry the gap</td></tr>
<tr><td>+ own declaration</td><td><code>respondents.csv</code> + declaration</td><td>measures the falsification channel directly: the share whose declaration differs from their attitude</td></tr>
<tr><td>+ perceived attitude of each contact, contacts anonymous</td><td><code>ego_network.csv</code>: respondent_id, neighbor_slot, perceived_neighbor_attitude</td><td>the edge-end test: mean perceived attitude over all reported contacts minus the degree-weighted mean of respondents' own attitudes equals the degree-weighted net misperception; exactly zero without misperception</td></tr>
<tr><td>+ each contact's number of contacts</td><td><code>ego_network.csv</code> + neighbor_degree</td><td>reach, hence both terms, without assuming neutral degree mixing</td></tr>
<tr><td>+ contacts' identities and true attitudes</td><td><code>ego_network.csv</code> + neighbor_id, neighbor_attitude</td><td>misperception rates per pair and per direction; the benchmark</td></tr>
</table>
<p>Each exported run also carries <code>ground_truth.json</code> (population quantities, both terms, the verdict, convergence), <code>params.json</code>, <code>trajectory.csv</code>, and optionally <code>state.rds</code> (attitudes, declarations, conformity parameters, edge list) so that any other sample size or design can be regenerated later. A batch adds <code>manifest.csv</code> with one row per run.</p>

<h4>7. Extensions (advanced options, off by default)</h4>
<ul>
<li><em>Homophily rewiring</em> \\(h\\): degree-preserving edge swaps that raise the share of same-attitude ties toward a target fraction \\(h\\) of edges.</li>
<li><em>Internalization</em>: each round, an agent declaring against its attitude adopts the declaration as its attitude with this probability; a run then counts as converged only when a quiet round leaves no such agent.</li>
<li><em>Sincere initial declarations</em> for the random start; <em>elicitation noise</em> on perceived prevalence at export; other network families (random, lattice, small world, core-periphery, complete, ring, star).</li>
</ul>
")

# ------------------------------------------------------------------ UI

sl <- function(id, label, min, max, value, step) sliderInput(id, HTML(label), min = min, max = max, value = value, step = step, width = "100%")
h5s <- function(x) tags$h5(x, style = "margin-top: 22px; font-weight: 500; color: #444;")

sidebar <- sidebarPanel(
  width = 4,
  actionButton("run", "RUN", class = "btn-primary", width = "100%"),
  h5s("Scenario"),
  radioButtons("scenario", NULL, c("Conformity from a random start" = "S1", "Well-connected minority" = "S2", "Private change of mind" = "S3")),
  conditionalPanel("input.scenario != 'S3'", sl("pi0", "Prevalence of attitude 1 \\(\\pi_0\\):", 0.5, 0.99, 0.6, 0.01)),
  conditionalPanel("input.scenario == 'S2'",
    checkboxInput("pure", HTML("No conformity (all \\(\\alpha_i = 0\\)): structure alone"), TRUE),
    selectInput("centrality", "Centrality measure:", c("degree", "eigenvector", "betweenness")),
    sl("lambda", "Centrality exponent \\(\\lambda\\) (0 = uncorrelated):", 0, 12, 4, 0.5)),
  conditionalPanel("input.scenario == 'S3'", sl("psi", "Fraction changing their mind \\(\\psi\\):", 0.5, 1, 0.7, 0.01)),
  h5s("Network"),
  selectInput("topology", "Network:", c("preferential attachment" = "ba", "random (Erdos-Renyi)" = "er", "regular lattice" = "regular",
                                        "small world" = "small_world", "core-periphery" = "core_periphery",
                                        "complete" = "complete", "ring" = "ring", "star" = "star")),
  sl("n", "Number of agents \\(n\\):", 10, 2000, 200, 10),
  conditionalPanel("input.topology == 'ba'", sl("m_ba", "Links per new agent \\(m\\):", 1, 10, 2, 1)),
  conditionalPanel("input.topology == 'er'", sl("mean_degree", "Mean number of contacts:", 1, 30, 6, 0.5)),
  conditionalPanel("input.topology == 'regular' || input.topology == 'small_world'", sl("k", "Contacts per agent \\(k\\) (even):", 2, 30, 4, 2)),
  conditionalPanel("input.topology == 'small_world'", sl("beta", "Rewiring probability \\(\\beta\\):", 0, 1, 0.1, 0.01)),
  conditionalPanel("input.topology == 'core_periphery'", sl("s_core", "Core size \\(s\\):", 2, 100, 10, 1), sl("k_out", "Periphery links into the core:", 1, 10, 1, 1)),
  conditionalPanel("!(input.scenario == 'S2' && input.pure)",
    h5s("Conformity"),
    selectInput("alpha_dist", HTML("Distribution of the conformity parameter \\(\\alpha_i\\):"), c("uniform on [0, 1]" = "uniform", "beta(a, b)" = "beta", "the same for everyone" = "point")),
    conditionalPanel("input.alpha_dist == 'beta'", fluidRow(column(6, numericInput("alpha_a", "a", 3, min = 0.01, step = 0.1)), column(6, numericInput("alpha_b", "b", 1, min = 0.01, step = 0.1)))),
    conditionalPanel("input.alpha_dist == 'point'", sl("alpha_point", "\\(\\alpha\\):", 0, 1, 0.5, 0.01))),
  h5s("Replication"),
  numericInput("seed", "Seed (replicate r uses seed + r - 1):", 1, min = 0, step = 1),
  sl("replicates", "Replicates:", 1, 100, 1, 1),
  checkboxInput("advanced", "Show advanced options", FALSE),
  conditionalPanel("input.advanced",
    conditionalPanel("input.scenario == 'S1'", radioButtons("init_decl", "Initial declarations:", c("random", "sincere"), inline = TRUE)),
    sl("homophily", "Homophily rewiring \\(h\\) (degree preserving; 0 = off):", 0, 1, 0, 0.05),
    sl("internalization", "Internalization rate (0 = attitudes fixed):", 0, 1, 0, 0.01),
    numericInput("max_rounds", "Round cap:", 500, min = 1, step = 1))
)

run_tab <- tabPanel("Run",
  fluidRow(style = "padding: 20px 0 10px 0;", column(12, tableOutput("summary"))),
  fluidRow(
    column(6, tags$p(tags$b("Network"), " (fill = attitude, ring = declaration). ",
                     numericInput("draw_max_n", "Draw up to n =", 300, min = 10, step = 10, width = "160px")),
           plotOutput("net_plot", height = "480px")),
    column(6, plotOutput("decomp_plot", height = "540px"))),
  fluidRow(style = "padding-top: 30px;",
    column(6, tags$p(tags$b("Mechanism square"), " over a cached cloud of runs from every scenario. ",
                     numericInput("bg_n", "Cloud n per run", 150, min = 30, step = 10, width = "160px"),
                     actionButton("bg_recompute", "Recompute cloud")),
           plotOutput("square_plot", height = "480px")),
    column(6, plotOutput("traj_plot", height = "540px"))),
  fluidRow(style = "padding-top: 30px;", column(12, tags$p(tags$b("Ground truth per replicate")), DTOutput("rep_table")))
)

export_tab <- tabPanel("Export data",
  fluidRow(style = "padding-top: 20px;",
    column(4, wellPanel(
      sl("m", "Respondents \\(m\\) (capped at \\(n\\)):", 10, 2000, 100, 10),
      numericInput("survey_seed", "Sampling seed:", 1, min = 0, step = 1),
      checkboxGroupInput("records", "What the survey records (attitude and perceived prevalence always):",
                         choices = setNames(names(SURVEY_OPTIONS), SURVEY_OPTIONS), selected = names(SURVEY_OPTIONS)),
      sl("noise_sd", "Elicitation noise on perceived prevalence (sd; 0 = none):", 0, 0.3, 0, 0.01),
      checkboxInput("write_state", "Include state.rds (full fixed point; any sample or design can be regenerated)", TRUE),
      downloadButton("download_zip", "Download zip (all replicates)", class = "btn-primary", style = "width: 100%"))),
    column(8,
      tags$p(tags$b("Edge-end test on this sample"), " (replicate 1; needs the anonymous contact perceptions):"),
      verbatimTextOutput("edge_end_text"),
      selectInput("preview_file", "Preview:", c("respondents.csv" = "respondents", "ego_network.csv" = "ego_network"), width = "220px"),
      DTOutput("preview_table")))
)

batch_tab <- tabPanel("Batch",
  fluidRow(style = "padding-top: 20px;",
    column(5, wellPanel(
      p("Comma-separated lists expand into a full factorial grid per scenario. Conformity specs: ", code("uniform; beta(3,1); point(0.5)"), "."),
      checkboxGroupInput("b_scenarios", "Scenarios:", c("Random start" = "S1", "Well-connected minority, no conformity" = "S2-pure",
                                                        "Well-connected minority, with conformity" = "S2-mixed", "Private change of mind" = "S3"),
                         selected = c("S1", "S2-pure", "S3")),
      textInput("b_n", "Number of agents n:", "200"),
      checkboxGroupInput("b_topos", "Networks:", c("preferential attachment" = "ba", "random" = "er", "regular lattice" = "regular", "small world" = "small_world",
                                                    "core-periphery" = "core_periphery", "complete" = "complete", "ring" = "ring", "star" = "star"), selected = c("ba"), inline = TRUE),
      fluidRow(column(4, textInput("b_m", "preferential: m", "2, 4")), column(4, textInput("b_mean_degree", "random: mean contacts", "4, 8")), column(4, textInput("b_k", "lattice, small world: k", "4, 8"))),
      fluidRow(column(4, textInput("b_beta", "small world: beta", "0.1")), column(4, textInput("b_s", "core-periphery: s", "10")), column(4, textInput("b_kout", "core-periphery: k_out", "1"))),
      fluidRow(column(4, textInput("b_pi0", "pi_0", "0.6, 0.7, 0.8")), column(4, textInput("b_lambda", "lambda", "0, 2, 8")), column(4, textInput("b_psi", "psi", "0.6, 0.7, 0.8"))),
      textInput("b_alpha", "Conformity specs:", "uniform; beta(3,1)"),
      selectInput("b_centrality", "Centrality measure (well-connected minority):", c("degree", "eigenvector", "betweenness")),
      fluidRow(column(6, textInput("b_homophily", "Homophily h", "0")), column(6, textInput("b_internalization", "Internalization", "0"))),
      fluidRow(column(6, numericInput("b_seeds", "Seeds per cell", 5, min = 1, step = 1)), column(6, numericInput("b_seed_start", "First seed", 1, min = 0, step = 1))),
      textInput("b_m_list", "Respondents per run (census = all agents):", "50, census"),
      checkboxInput("b_state", "Write state.rds per run", TRUE),
      fileInput("b_yaml", "Or load a grid YAML (overrides the fields above):", accept = c(".yaml", ".yml")),
      textInput("b_out", "Output directory:", file.path(root, "corpora", format(Sys.time(), "corpus-%Y%m%d-%H%M"))),
      fluidRow(column(6, actionButton("b_preview", "Preview grid", width = "100%")), column(6, actionButton("b_run", "Run sweep", class = "btn-primary", width = "100%"))))),
    column(7,
      verbatimTextOutput("b_preview_text"),
      plotOutput("b_square", height = "420px"),
      DTOutput("b_manifest")))
)

guide_tab <- tabPanel("Guide", fluidRow(style = "padding-top: 20px;", column(11, guide_html)))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 3, bootswatch = "paper"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;} pre.shiny-text-output {white-space: pre-wrap;} .table-summary td, .table-summary th {padding: 4px 14px;}"),
  titlePanel("Social Networks and Pluralistic Ignorance"),
  withMathJax(),
  description_band,
  sidebarLayout(
    sidebar,
    mainPanel(width = 8, fluidRow(style = "padding-left: 20px; padding-bottom: 40px;",
      tabsetPanel(type = "tabs", run_tab, export_tab, batch_tab, guide_tab)))
  )
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
  p$alpha_dist <- input$alpha_dist; p$alpha_a <- input$alpha_a; p$alpha_b <- input$alpha_b; p$alpha_point <- input$alpha_point
  p$lambda <- input$lambda; p$centrality <- input$centrality; p$psi <- input$psi
  p$homophily <- input$homophily; p$internalization <- input$internalization
  p$max_rounds <- as.integer(input$max_rounds); p$seed <- as.integer(input$seed)
  p
}

background_cloud <- function(n_bg, progress = NULL) {
  f <- file.path(cache_dir, sprintf("bg_cloud_v2_n%d.rds", n_bg))
  if (file.exists(f)) return(readRDS(f))
  specs <- list()
  for (s in 1:25) specs[[length(specs) + 1]] <- list(scenario = "S1", n = n_bg, topology = "ba", m = 2, pi0 = 0.6, seed = s)
  for (s in 1:25) specs[[length(specs) + 1]] <- list(scenario = "S1", n = n_bg, topology = "ba", m = 2, pi0 = 0.6, alpha_dist = "beta", alpha_a = 3, alpha_b = 1, seed = 100 + s)
  for (lam in c(0, 1, 2, 4, 8)) for (s in 1:5) {
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = TRUE, seed = 2000 + 10 * lam + s)
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = FALSE, alpha_dist = "beta", alpha_a = 3, alpha_b = 1, seed = 6000 + 10 * lam + s)
  }
  for (psi in c(0.55, 0.65, 0.75, 0.85)) for (s in 1:6) specs[[length(specs) + 1]] <- list(scenario = "S3", n = n_bg, topology = "regular", k = 8, psi = psi, alpha_dist = "beta", alpha_a = 3, alpha_b = 1, seed = 9000 + round(100 * psi) + s)
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
      data.frame(scenario = scenario_name(x$st$params), seed = x$st$params$seed, prevalence = gt$p, majority_perceived_share = gt$majority_mean_perceived_share,
                 pluralistic_ignorance = gt$pluralistic_ignorance, gap = gt$gap, misperception = gt$term_misperception, structure = gt$term_structure,
                 mis_maj = mf$misperception, str_maj = mf$structure, falsified_share = gt$falsified_share,
                 edge_end_net_misperception = gt$deg_weighted_net_falsification, converged = gt$converged, rounds = gt$rounds,
                 mean_degree = gt$mean_degree, degree_assortativity = gt$degree_assortativity, edge_homophily = gt$edge_homophily, n_internalized = gt$n_internalized)
    }))
  })

  fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")
  output$summary <- renderTable({
    gt <- first()$gt
    data.frame(
      check.names = FALSE,
      "Majority attitude" = as.character(gt$majority_attitude),
      "Its true share" = fmt(gt$majority_share),
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
      scale_fill_manual(values = c(PI_COL$black, PI_COL$orange, PI_COL$blue)) +
      scale_y_continuous(expand = expansion(mult = 0.2)) +
      labs(x = NULL, y = "Perceived minus true prevalence (majority frame)",
           subtitle = sprintf("Gap = misperception + structure (residual %.1e).", gt$decomposition_residual)) + theme_pi()
  })

  bg <- reactiveVal(NULL)
  observeEvent(input$bg_recompute, {
    f <- file.path(cache_dir, sprintf("bg_cloud_v2_n%d.rds", as.integer(input$bg_n))); if (file.exists(f)) file.remove(f)
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
      geom_hline(yintercept = 0.5, linetype = 2, color = PI_COL$grey) +
      scale_color_manual(values = c(PI_COL$blue, PI_COL$black)) + ylim(0, 1) +
      labs(x = "Round", y = "Share", subtitle = outcome) + theme_pi()
  })

  output$rep_table <- renderDT({
    tb <- gt_table()[, c("scenario", "seed", "prevalence", "majority_perceived_share", "pluralistic_ignorance", "gap", "misperception", "structure", "falsified_share", "edge_end_net_misperception", "converged", "rounds", "mean_degree", "degree_assortativity", "edge_homophily", "n_internalized")]
    datatable(tb, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("prevalence", "majority_perceived_share", "gap", "misperception", "structure", "falsified_share", "edge_end_net_misperception", "mean_degree", "degree_assortativity", "edge_homophily"), 3)
  })

  # ---- export
  sv1 <- reactive({ st <- first()$st; survey(st, input$m, input$survey_seed, input$noise_sd) })
  output$preview_table <- renderDT({
    ds <- apply_design(sv1(), input$records)
    tab <- if (input$preview_file == "respondents") ds$respondents else ds$ego_network
    if (is.null(tab)) return(datatable(data.frame(note = "ego_network.csv is not produced unless contact perceptions are recorded."), rownames = FALSE))
    datatable(tab, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$edge_end_text <- renderText({
    sv <- sv1(); gt <- first()$gt
    if (!("ego_perceptions" %in% input$records)) return("Record the perceived attitude of each contact to run the edge-end test.")
    est <- edge_end_estimates(sv$respondents, sv$ego_network); bb <- edge_end_bootstrap(sv$respondents, sv$ego_network, B = 300)
    sprintf("Respondents m = %d.\nPerceived edge-end prevalence q\u0302 = %.4f;  true edge-end prevalence estimate p\u0303 = %.4f;\nedge-end test q\u0302 \u2212 p\u0303 = %.4f  [95%% bootstrap CI %.4f, %.4f].\nGround truth (census): degree-weighted net misperception = %.4f.",
            sv$m, est$q_hat, est$p_tilde_hat, est$net_misperception, bb$ci_lower, bb$ci_upper, gt$deg_weighted_net_falsification)
  })
  output$download_zip <- downloadHandler(
    filename = function() sprintf("pi_%s_n%d_seed%d.zip", input$scenario, input$n, input$seed),
    content = function(file) {
      tmp <- tempfile("pi_"); dir.create(tmp)
      for (i in seq_along(runs())) {
        st <- runs()[[i]]$st; sv <- survey(st, input$m, input$survey_seed + i - 1L, input$noise_sd)
        write_run(st, sv, file.path(tmp, sprintf("run_%03d_seed%d", i, st$params$seed)), include = input$records, write_state = input$write_state)
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
                homophily = parse_num_list(input$b_homophily), internalization = parse_num_list(input$b_internalization))
      if (sc != "S3") b$pi0 <- parse_num_list(input$b_pi0)
      if (sc %in% c("S2-pure", "S2-mixed")) { b$lambda <- parse_num_list(input$b_lambda); b$centrality <- input$b_centrality }
      if (sc == "S3") b$psi <- parse_num_list(input$b_psi)
      if (sc != "S2-pure") b$alpha <- split_specs(input$b_alpha)
      b
    })
    list(name = basename(input$b_out), seeds_per_cell = as.integer(input$b_seeds), m = as.list(trimws(unlist(strsplit(input$b_m_list, ",")))), blocks = blocks)
  })
  preview_text <- reactiveVal("Press Preview grid.")
  observeEvent(input$b_preview, {
    spec <- batch_spec(); cells <- expand_grid_spec(spec); seeds <- spec[["seeds_per_cell"]]
    tab <- table(vapply(cells, scenario_name, character(1)))
    preview_text(sprintf("%d cells x %d seeds = %d runs (%s). Rough runtime %.1f min. Output: %s",
                         length(cells), seeds, length(cells) * seeds, paste(sprintf("%s: %d", names(tab), tab), collapse = "; "),
                         estimate_runtime_seconds(cells, seeds, length(spec[["m"]])) / 60, input$b_out))
  })
  manifest <- reactiveVal(NULL)
  observeEvent(input$b_run, {
    spec <- batch_spec(); cells <- expand_grid_spec(spec)
    withProgress(message = "Running sweep", value = 0, {
      mf <- run_sweep(cells, input$b_out, seeds_per_cell = spec[["seeds_per_cell"]], seed_start = as.integer(input$b_seed_start), m_list = spec[["m"]],
                      write_state = input$b_state, progress = function(d, t) setProgress(d / t, detail = sprintf("%d / %d", d, t)), name = spec$name)
    })
    yaml::write_yaml(spec, file.path(input$b_out, "grid.yaml"))
    manifest(mf); preview_text(sprintf("Done: %d runs written to %s (manifest.csv, grid.yaml, corpus.json).", nrow(mf), input$b_out))
  })
  output$b_preview_text <- renderText(preview_text())
  output$b_manifest <- renderDT({ req(manifest()); datatable(manifest(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$b_square <- renderPlot({
    req(isTRUE(session$clientData$output_b_square_width > 0), isTRUE(session$clientData$output_b_square_height > 0))
    mf <- manifest(); req(mf)
    s <- ifelse(mf$p >= 0.5, 1, -1)
    d <- data.frame(class = mf$label, structure = s * mf$term_structure, misperception = s * mf$term_misperception)
    square_plot(d) + labs(subtitle = "Corpus coverage of the mechanism square")
  })
}

shinyApp(ui, server)
