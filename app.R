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

SCENARIO_CHOICES <- c("1. Conformity from a random start" = "S1", "2. Private change of mind" = "S3", "3. Well-connected minority (friendship paradox)" = "S2")

# ------------------------------------------------------------------ text
description_band <- fluidRow(
  style = PI_BAND_STYLE,
  column(width = 4, div(HTML(
    "<strong>What this tool is for.</strong> Surveys often find that many people privately hold an attitude but believe that most of the people around them hold the opposite one. Two very different things can lie behind such data. In one, people are wrong about the people around them, because those people say one thing in public and think another in private. In the other, people are right about the people around them, and it only looks like a mistake from the outside, because well-connected people are seen by many and tend to hold the less common attitude. This tool simulates both, produces survey datasets from them, and keeps a key, so that statistical methods for telling them apart can be tested.
    </br></br>
    <strong>The model.</strong> There are \\(N\\) agents connected by a network with adjacency matrix \\(\\mathbf{A}\\); agent \\(i\\)'s contacts are \\(\\mathcal{N}_i\\), and \\(d_i = |\\mathcal{N}_i|\\) is their number. Each agent \\(i\\) has three things: a binary private attitude \\(Y_i \\in \\{0,1\\}\\), which is what it really thinks; a binary public declaration \\(X_i \\in \\{0,1\\}\\), which is what it says; and a conformity parameter \\(c_i \\in [0,1]\\), which is how much it cares about agreeing with its contacts rather than saying what it thinks. Declaring \\(x\\) pays
    \\[U_i(x) = c_i \\, S_i(x) + (1-c_i) \\, \\mathbf{1}[x = Y_i],\\]
    where \\(S_i(x)\\) is the share of \\(i\\)'s contacts that \\(i\\) takes to hold \\(x\\). An agent with \\(c_i = 0\\) always says what it thinks; an agent with \\(c_i = 1\\) always says what most of its contacts seem to think. Agents take turns, in a random order each round, choosing the declaration that pays more, until a whole round passes with no change. Attitudes never change; only declarations do. Agents take their contacts at their word: writing \\(P_{ij}\\) for the attitude \\(i\\) believes contact \\(j\\) holds, \\(P_{ij} = X_j\\).
    </br></br>
    <strong>Open-Source Code:</strong> The <a href='https://www.r-project.org/'>R</a> and <a href='https://shiny.rstudio.com/'>Shiny</a> code is available at <a href='https://github.com/amohseni/Social-Networks-and-Pluralistic-Ignorance'>www.github.com/amohseni</a>."))),
  column(width = 4, div(HTML(
    "<strong>Pluralistic ignorance.</strong> This is a state of an individual agent, judged against that agent's own contacts. Agent \\(i\\) is in a state of pluralistic ignorance when three things are true at once: \\(i\\) believes that most of its contacts hold the opposite attitude to its own; \\(i\\) is wrong about that, because in fact at least half of its contacts share its attitude; and \\(i\\) nevertheless declares the opposite attitude, going along with what it wrongly takes to be the local majority. In symbols, the three conditions are
    \\[\\frac{1}{d_i}\\sum_{j \\in \\mathcal{N}_i} \\mathbf{1}[P_{ij} \\neq Y_i] > \\tfrac{1}{2}, \\qquad \\frac{1}{d_i}\\sum_{j \\in \\mathcal{N}_i} \\mathbf{1}[Y_j \\neq Y_i] \\le \\tfrac{1}{2}, \\qquad X_i \\neq Y_i.\\]
    It can happen to holders of either attitude, and to both at once in the same population. The prevalence of pluralistic ignorance in a population is the share of all agents in that state. The quantity the analysts are asked to estimate is this prevalence.
    </br></br>
    <strong>Why the data can mislead.</strong> Only the first condition is visible in a survey: a respondent says that most of their contacts hold the other attitude. The second condition, whether that belief is wrong, is not visible, and here the network matters. On average, a person's contacts have more contacts than the person does, so well-connected people appear in a great many people's circles. If the well-connected people tend to hold the less common attitude, then most people, quite correctly, see a circle in which their own attitude is the minority. Their beliefs are accurate, nobody is in a state of pluralistic ignorance, and yet the survey looks exactly as it would if everyone were. Whether conformity then also leads them to declare the other attitude is a separate matter, which is why scenario 3 comes in three versions."))),
  column(width = 4, div(HTML(
    "<strong>Scenarios.</strong>
    <ol>
      <li><strong>Conformity from a random start.</strong> Attitudes are assigned at random with a chosen majority, and declarations start at random too. Conformity then reshapes the declarations round by round. Pluralistic ignorance arises where an agent's contacts share its attitude but happen to be declaring the other one.</li>
      <li><strong>Private change of mind.</strong> Everyone starts out holding and declaring attitude 1. Then a majority of agents privately switch to attitude 0, while everyone is still declaring 1. Agents who switched see only declarations of 1 around them, wrongly conclude that they are alone, and, if conformity is strong enough, keep declaring 1: pluralistic ignorance among the new majority.</li>
      <li><strong>Well-connected minority.</strong> The less common attitude is placed on the most central agents. Three versions: with conformity to declarations, agents respond to what their contacts say, as in the other scenarios; with conformity to true attitudes, agents can see what their contacts really think and conform to that; with conformity off, everyone declares sincerely. In the last two versions no agent is ever wrong about its contacts, so the prevalence of pluralistic ignorance is exactly zero, even though the data show the pattern.</li>
    </ol>
    <strong>How to use it.</strong> <em>Generate corpus</em> builds a set of survey datasets from the scenarios you choose, all of which show the pattern, written in each data configuration you tick (C1 to C5: from attitude and perceived prevalence alone up to a census with the full network), and a key with the true prevalence in each. Give the analysts the datasets; keep the key. <em>Explore</em> runs one scenario at a time and shows what happens inside the population.")))
)

config_table_html <- function() {
  rows <- vapply(CONFIG_CODES, function(code) {
    cs <- config_summary(code, add_pn = FALSE)
    est <- switch(code,
      C1 = "the perception discrepancy \\(\\Delta_G = \\mu_P - \\mu_Y\\) only; the two processes cannot be told apart from these columns alone",
      C2 = "whether \\(X_i\\) departs from \\(Y_i\\) (the conformity condition of the definition)",
      C3 = "\\(\\mathrm{Cov}(D, Y)\\), the covariance between attitude and number of contacts, which the friendship-paradox process requires; the budget tests for that process alone",
      C3a = "\\(\\eta\\), the degree-weighted perception error: the mean of \\(P_{ij}\\) over all reported contacts minus the degree-weighted mean of the respondents' own \\(Y_i\\); zero when beliefs about contacts are accurate",
      C4 = "the perception error \\(P_{\\mathcal{N}_i} - \\mu_{Y,\\mathcal{N}_i}\\) respondent by respondent, and the exposure bias \\(\\mu_{Y,\\mathcal{N}_i} - \\mu_Y\\); the first configuration at which the two processes are separated",
      C5 = "everything exactly; the reference point and the simulation ground truth")
    sprintf("<tr><td><b>%s</b></td><td>%s</td><td>%s</td></tr>", code, paste(sprintf("<code>%s</code>", cs$files), collapse = "<br>"), est)
  }, character(1))
  paste0("<table class='table table-condensed' style='max-width: 1100px;'><tr><th>Configuration</th><th>Files and columns</th><th>What can be estimated</th></tr>", paste(rows, collapse = ""), "</table>")
}

guide_html <- HTML("
<h4>0. Notation</h4>
<p>The symbols follow the project's problem statement (<em>Pluralistic Ignorance and the Friendship Paradox</em>, September 2026). \\(\\mathbf{A}\\) is the adjacency matrix of the population of \\(N\\) agents; \\(n\\) is the number of respondents in a survey. For agent \\(i\\): \\(Y_i\\) is the private attitude, \\(X_i\\) the public declaration (the problem statement's outward behavior), \\(P_i\\) the perceived share of the whole population holding attitude 1, \\(P_{\\mathcal{N}_i}\\) the perceived share among \\(i\\)'s own contacts, \\(\\mathcal{N}_i\\) the set of contacts, and \\(d_i = |\\mathcal{N}_i|\\) their number. Population means are \\(\\mu_Y\\), \\(\\mu_X\\), \\(\\mu_P\\); local means over \\(i\\)'s contacts are \\(\\mu_{Y,\\mathcal{N}_i}\\) and \\(\\mu_{X,\\mathcal{N}_i}\\); \\(\\Delta_G = \\mu_P - \\mu_Y\\) is the perception discrepancy. Two symbols are additions for the simulation: \\(P_{ij}\\), the attitude \\(i\\) believes contact \\(j\\) holds (so \\(P_{\\mathcal{N}_i} = \\tfrac{1}{d_i}\\sum_{j \\in \\mathcal{N}_i} P_{ij}\\)), and \\(c_i\\), the conformity parameter. The columns of the exported files use plain names: \\(Y_i\\) is <code>attitude</code>, \\(X_i\\) is <code>declaration</code>, \\(P_i\\) is <code>perceived_prevalence</code>, \\(P_{\\mathcal{N}_i}\\) is <code>perceived_contacts_mean</code>, \\(d_i\\) is <code>degree</code>, \\(P_{ij}\\) is <code>perceived_neighbor_attitude</code>; the key uses <code>mu_Y</code>, <code>mu_P</code>, <code>Delta_G</code>, and so on.</p>

<h4>1. The model</h4>
<p>There are \\(N\\) agents on an undirected network. Each agent \\(i\\) has a binary private attitude \\(Y_i \\in \\{0,1\\}\\), a binary public declaration \\(X_i \\in \\{0,1\\}\\), and a conformity parameter \\(c_i \\in [0,1]\\) drawn from a chosen distribution. The payoff of declaring \\(x\\) is \\(U_i(x) = c_i S_i(x) + (1-c_i)\\,\\mathbf{1}[x = Y_i]\\), where \\(S_i(x) = \\frac{1}{d_i}\\sum_{j \\in \\mathcal{N}_i} \\mathbf{1}[P_{ij} = x]\\) is the share of \\(i\\)'s contacts that \\(i\\) takes to hold \\(x\\). An agent therefore declares its own attitude when \\((1-c_i) \\ge c_i\\,(1 - 2S_i(Y_i))\\) and the other attitude otherwise; when the two payoffs are equal it declares its own attitude. In each round, agents update one at a time in a random order. The process stops at the first round in which no declaration changes. If no such round occurs within the round cap, the run is recorded as not converged. Attitudes do not change during a run.</p>
<p>What an agent takes its contacts to hold is, in every scenario but one, what they declare: \\(P_{ij} = X_j\\). In the clairvoyant version of scenario 3 it is what they really hold: \\(P_{ij} = Y_j\\). A respondent's perceived prevalence \\(P_i\\) in the survey is the share of its contacts it takes to hold attitude 1, so \\(P_i = P_{\\mathcal{N}_i}\\) unless extrapolation noise is added at survey time.</p>

<h4>2. Definition of pluralistic ignorance</h4>
<p>Pluralistic ignorance is a state of an individual agent, judged against that agent's own contacts rather than against the whole population. Agent \\(i\\) is in that state when three conditions hold: \\(i\\) believes that a majority of its contacts hold the attitude opposite to its own, \\(\\frac{1}{d_i}\\sum_{j \\in \\mathcal{N}_i} \\mathbf{1}[P_{ij} \\neq Y_i] > 1/2\\); that belief is mistaken, \\(\\frac{1}{d_i}\\sum_{j \\in \\mathcal{N}_i} \\mathbf{1}[Y_j \\neq Y_i] \\le 1/2\\); and \\(i\\) declares the opposite attitude, \\(X_i \\neq Y_i\\). Holders of either attitude can be in this state, and both can be at the same time in one population: an agent holding 1 whose contacts hold 1 but declare 0, and an agent holding 0 whose contacts hold 0 but declare 1, are both in a state of pluralistic ignorance. The prevalence of pluralistic ignorance is the share of all agents in the state; the tool also reports it separately for holders of each attitude.</p>
<p>The pattern that motivates the question is the first condition on its own: an agent believes that its own attitude is the minority one among its contacts. A dataset shows the pattern when more than half of the agents (and of the respondents) believe this. Every dataset in a generated corpus shows the pattern. The prevalence of pluralistic ignorance in it can be anything from zero upward, and estimating it is the analysts' task.</p>

<h4>3. Why the pattern can appear without pluralistic ignorance</h4>
<p>Suppose every declaration is sincere and every belief about a contact is correct. On average, an agent's contacts have more contacts than the agent does (the friendship paradox), so well-connected agents are part of a large share of all circles. If the well-connected agents tend to hold the less common attitude, then most agents have circles in which their own attitude is the minority, and they believe so correctly. The pattern is present, but the second condition of the definition fails for everyone, so the prevalence of pluralistic ignorance is zero. A star network is the limiting case: the center holds attitude 0, every other agent holds 1 and has exactly one contact, the center, so every non-center agent correctly believes that all of its contacts hold 0.</p>

<h4>4. The decomposition of the perception discrepancy</h4>
<p>The problem statement writes each respondent's error as \\(P_i - \\mu_Y = (\\mu_{Y,\\mathcal{N}_i} - \\mu_Y) + (P_{\\mathcal{N}_i} - \\mu_{Y,\\mathcal{N}_i}) + (P_i - P_{\\mathcal{N}_i})\\): the exposure bias, the perception error, and the extrapolation error. The key reports the first two averaged over agents, as <code>exposure_bias</code> and <code>perception_error</code>; their sum is \\(\\Delta_G\\), since the extrapolation error is zero in the model. The exposure bias is nonzero only when attitude is correlated with how often an agent appears in others' circles, which is the mechanism of the friendship-paradox process, and it is zero on any regular network. The perception error is nonzero only when some declarations differ from attitudes, which is the mechanism of pluralistic ignorance. It is a net quantity: errors in opposite directions cancel in it, so the key also reports <code>perception_error_gross</code>, the share of all contact perceptions that are wrong. The exposure distortion of behavior, \\(\\Delta_F = \\frac{1}{N}\\sum_i \\mu_{X,\\mathcal{N}_i} - \\mu_X\\), is reported as <code>Delta_F</code>. The statistic \\(\\eta = \\sum_i d_i (P_{\\mathcal{N}_i} - Y_i) / \\sum_i d_i\\), the degree-weighted perception error proposed in the problem statement, is reported as <code>eta</code> for the population and as <code>eta_sample_as_coded</code> computed on the respondents; it is zero whenever every agent perceives its contacts accurately.</p>

<h4>5. What the plots show</h4>
<p>In the Explore tab, the bar chart reports, for the current run, the share of agents who believe their attitude is the local minority, the share whose belief about which attitude most of their contacts hold is wrong, the share declaring against their attitude, and the prevalence of pluralistic ignorance, overall and by attitude. The scatter plot places the run among a stored set of runs from every scenario version, with the share believing they are in the local minority on the horizontal axis and the prevalence of pluralistic ignorance on the vertical axis: runs from the accurate-perception versions lie on the horizontal axis. The line chart shows the share declaring 1 and the share holding 1 in each round. The same scatter plot in the Generate corpus tab shows the generated datasets.</p>

<h4>6. The scenarios</h4>
<ol>
<li><strong>Conformity from a random start.</strong> Attitudes are drawn independently with a chosen prevalence \\(\\pi_0\\) of attitude 1 (the expected value of \\(\\mu_Y\\)), and declarations start at random. Conformity then changes the declarations. Pluralistic ignorance arises for agents whose contacts mostly share their attitude but are declaring the other one; the more the conformity parameters are concentrated near \\(1\\), the more of it there is.</li>
<li><strong>Private change of mind.</strong> Every agent starts out holding and declaring attitude \\(1\\). Then a share \\(\\psi > 1/2\\) of agents changes attitude to \\(0\\) while every declaration is still \\(1\\). Each agent that changed declares \\(0\\) only if its conformity parameter is low enough given what its contacts are saying. Either the declarations change to \\(0\\) across the population, or they stay at \\(1\\); in the second case the agents that changed, most of whose contacts also changed, believe they are alone and keep declaring \\(1\\): pluralistic ignorance.</li>
<li><strong>Well-connected minority.</strong> A share \\(1 - \\pi_0\\) of agents receives attitude \\(0\\), chosen with probability proportional to centrality raised to the power \\(\\lambda\\), using one centrality measure for the whole corpus (degree by default). Three versions. <em>Conformity to declarations</em>: agents conform to what contacts declare, as in the other scenarios; peripheral agents may come to declare the central agents' attitude and central agents the peripheral agents' attitude, and some agents can then be in a state of pluralistic ignorance. <em>Conformity to true attitudes</em>: agents can see what their contacts really hold and conform to that; beliefs are accurate, so the prevalence is zero, although many agents declare against their attitude. <em>Conformity off</em>: \\(c_i = 0\\) for every agent; declarations are sincere, beliefs accurate, prevalence zero.</li>
</ol>

<h4>7. The generated corpus and the key</h4>
<p>The <em>Generate corpus</em> tab produces a set of datasets. Each comes from one run of one scenario version, and only runs that show the pattern, in the population and in the sample of respondents, are kept; the other runs are discarded and counted. The number of datasets per scenario version is chosen by the user. For each dataset, the parameters are drawn at random from ranges that can be changed under the advanced options: population size \\(N\\), contacts per new agent, the majority share, the conformity distribution (\\(c_i \\sim \\mathrm{beta}(a, 1)\\), one range of \\(a\\) for every scenario), and the centrality exponent. One centrality measure and one network family are used for the whole corpus, so that neither differs between the processes. The family is preferential attachment (Barabási-Albert) by default; a random graph (Erdős-Rényi) or a small world (Watts-Strogatz) can be chosen instead, with their own parameter ranges. The coding of attitudes as \\(0\\) and \\(1\\) is reversed in a random half of the datasets, respondent identifiers are assigned at random, and dataset numbers are assigned at random.</p>
<p>The files for the analysts are written once per data configuration (section 8), each configuration in its own folder with a <code>README.txt</code> codebook, <code>respondents.csv</code> (one row per respondent, all datasets in one file, identified by the column <code>dataset_id</code>), and, where the configuration records contacts, <code>ego_network.csv</code> (one row per respondent-contact pair) and <code>network_edges.csv</code> (one row per tie). The key, downloaded separately, is <code>key.csv</code>: one row per dataset with the process (genuine pluralistic ignorance or friendship paradox), the scenario version, every parameter, \\(N\\) and \\(n\\), the prevalence of pluralistic ignorance (overall and for each attitude as coded), the share of agents who believe they are in the local minority, the share declaring against their attitude, the exposure bias and the perception error, \\(\\Delta_F\\), and \\(\\eta\\) for the population and on the sample. Two further files hold the full final state of every population: <code>nodes.csv</code> (one row per agent, numbered as in <code>respondent_id</code>, with \\(Y_i\\), \\(X_i\\), \\(P_i\\), \\(P_{\\mathcal{N}_i}\\), \\(d_i\\), \\(c_i\\), whether the agent was sampled, whether it answered when named as a contact, whether it believes it is in the local minority, and whether it is in a state of pluralistic ignorance) and <code>edges.csv</code> (one row per tie).</p>

<h4>8. Data configurations: what the survey records</h4>
<p>A data configuration is what a study records about each respondent. The five configurations below are those of the problem statement (board of 27 September), in order of increasing information, with one addition, C3a, that sits between C3 and C4. \\(P_{\\mathcal{N}_i}\\) can be added to any configuration as the item <code>perceived_contacts_mean</code>; the contacts' \\((X_j, Y_j, d_j)\\) are <code>neighbor_declaration</code>, <code>neighbor_attitude</code>, <code>neighbor_degree</code> in <code>ego_network.csv</code>; the global network \\(\\mathbf{A}\\) is <code>network_edges.csv</code>. Every configuration is a nested view of the same datasets, with the same identifiers, so analysts can receive them one at a time.</p>
", config_table_html(), "
<p>In this model respondents take the share among their contacts as their estimate for the population, so \\(P_i = P_{\\mathcal{N}_i}\\) and the extrapolation error of the decomposition is zero unless extrapolation noise is added (Generate corpus, parameter ranges). With the ego network, the contacts' own answers are present only for contacts who answered the survey; the share who answer can be set there too.</p>
")

# ------------------------------------------------------------------ UI

sl <- function(id, label, min, max, value, step) sliderInput(id, HTML(label), min = min, max = max, value = value, step = step, width = "100%")
h5s <- function(x) tags$h5(x, style = "margin-top: 22px; font-weight: 500; color: #444;")

hp <- function(...) helpText(..., style = "margin-top: -6px; margin-bottom: 14px; color: #777;")

explore_controls <- wellPanel(
  actionButton("run", "RUN", class = "btn-primary", width = "100%"),
  helpText("Runs the chosen scenario with the settings below and shows the result on the right.", style = "margin-bottom: 14px; color: #777;"),
  h5s("Scenario"),
  radioButtons("scenario", NULL, SCENARIO_CHOICES),
  hp("Scenarios 1 and 2 produce genuine pluralistic ignorance. Scenario 3 produces the same pattern in the data through the structure of the network."),
  conditionalPanel("input.scenario != 'S3'", sl("pi0", "Share of agents holding attitude \\(Y = 1\\), \\(\\pi_0\\):", 0.5, 0.99, 0.6, 0.01),
    hp("The rest hold attitude 0. Attitudes are assigned at random with this share, the expected value of \\(\\mu_Y\\).")),
  conditionalPanel("input.scenario == 'S3'", sl("psi", "Share of agents who change their mind \\(\\psi\\):", 0.5, 1, 0.7, 0.01),
    hp("Everyone starts out holding and declaring attitude 1; this share then switches privately to attitude 0 and becomes the majority.")),
  conditionalPanel("input.scenario == 'S2'",
    radioButtons("s2_mode", "Version:", c("conformity to declarations" = "declarations", "conformity to true attitudes" = "attitudes", "conformity off" = "off")),
    hp("What agents respond to when they conform: what their neighbors declare, what their neighbors really think, or nothing at all."),
    selectInput("centrality", "Centrality measure:", c("degree", "eigenvector", "betweenness")),
    hp("How central an agent is when the minority attitude is placed on central agents. Degree: number of contacts. Eigenvector: central in proportion to the centrality of one's contacts. Betweenness: share of shortest paths between other agents that pass through the agent."),
    sl("lambda", "Strength of the link between centrality and the minority attitude \\(\\lambda\\):", 0, 12, 4, 0.5),
    hp("Agents receive the minority attitude with probability proportional to centrality raised to this power. At 0 the minority attitude is placed at random; larger values concentrate it on the most central agents.")),
  conditionalPanel("!(input.scenario == 'S2' && input.s2_mode == 'off')",
    h5s("Conformity"),
    selectInput("c_dist", HTML("Distribution of the conformity parameter \\(c_i\\):"), c("beta(a, b)" = "beta", "uniform on [0, 1]" = "uniform", "the same for everyone" = "point")),
    hp("Each agent draws its own conformity parameter from this distribution. An agent with 0 always says what it thinks; an agent with 1 always says what most of its neighbors seem to think."),
    conditionalPanel("input.c_dist == 'beta'", fluidRow(column(6, numericInput("c_a", "a", 3, min = 0.01, step = 0.1)), column(6, numericInput("c_b", "b", 1, min = 0.01, step = 0.1))),
      hp("beta(a, b) lives on [0, 1] with mean a / (a + b). With a = 3 and b = 1 most agents are near 0.75; raising a moves them toward 1.")),
    conditionalPanel("input.c_dist == 'point'", sl("c_point", "\\(c\\):", 0, 1, 0.5, 0.01), hp("Every agent gets this value."))),
  h5s("Network"),
  selectInput("topology", "Network:", c("Preferential attachment (Barabási-Albert)" = "ba", "Random graph (Erdős-Rényi)" = "er", "Small world (Watts-Strogatz)" = "small_world",
                                        "Regular lattice" = "regular", "Core-periphery" = "core_periphery", "Complete" = "complete", "Ring" = "ring", "Star" = "star")),
  hp("How the network is built. Preferential attachment, the default, gives a few agents with very many contacts and many with few, as in most real social networks. The random graph gives everyone a similar number of contacts. The small world starts from a ring and rewires a share of ties."),
  sl("n", "Number of agents \\(N\\):", 10, 2000, 200, 10),
  hp("The size of the population."),
  conditionalPanel("input.topology == 'ba'", sl("m_ba", "Contacts made by each new agent \\(m\\):", 1, 10, 2, 1),
    hp("Agents are added one at a time and each new one links to this many existing agents, chosen in proportion to how many contacts they already have. The average number of contacts is about twice this.")),
  conditionalPanel("input.topology == 'er'", sl("mean_degree", "Average number of contacts:", 1, 30, 6, 0.5),
    hp("Every pair of agents is linked independently, with the probability that gives this average.")),
  conditionalPanel("input.topology == 'regular' || input.topology == 'small_world'", sl("k", "Contacts per agent \\(k\\) (even):", 2, 30, 4, 2),
    hp("Every agent has exactly this many contacts, arranged around a ring.")),
  conditionalPanel("input.topology == 'small_world'", sl("beta", "Share of ties rewired \\(\\beta\\):", 0, 1, 0.1, 0.01),
    hp("This share of the ring ties is reconnected to agents chosen at random.")),
  conditionalPanel("input.topology == 'core_periphery'", sl("s_core", "Core size \\(s\\):", 2, 100, 10, 1), sl("k_out", "Ties from each other agent into the core:", 1, 10, 1, 1),
    hp("The core agents are all linked to one another; every other agent links only to this many core agents.")),
  h5s("Replication"),
  numericInput("seed", "Seed:", 1, min = 0, step = 1),
  hp("Fixes the random numbers, so that the same settings and seed give exactly the same run."),
  sl("replicates", "Replicates:", 1, 100, 1, 1),
  hp("Number of runs, using this seed and the next ones in turn. The plots show the first run; the table at the bottom shows all of them."),
  checkboxInput("advanced", "Show advanced options", FALSE),
  conditionalPanel("input.advanced",
    conditionalPanel("input.scenario == 'S1'", radioButtons("init_decl", "Initial declarations:", c("random", "sincere"), inline = TRUE),
      hp("Whether declarations start at random or equal to attitudes.")),
    sl("homophily", "Homophily rewiring \\(h\\):", 0, 1, 0, 0.05),
    hp("Moves this share of ties toward pairs of agents with the same attitude, without changing anyone's number of contacts. 0 leaves the network as built."),
    sl("internalization", "Internalization rate:", 0, 1, 0, 0.01),
    hp("Each round, an agent declaring against its attitude adopts the declaration as its attitude with this probability. 0 keeps attitudes fixed."),
    numericInput("max_rounds", "Round cap:", 500, min = 1, step = 1),
    hp("A run that has not settled by this round is stopped and reported as not converged."),
    numericInput("draw_max_n", "Draw the network up to N =", 300, min = 10, step = 10),
    hp("Above this population size the network drawing is replaced by a histogram of the number of contacts \\(d_i\\)."),
    numericInput("bg_n", "Stored set of runs for the scatter plot: N per run", 150, min = 30, step = 10),
    hp("Population size of the runs that form the background of the scatter plot."),
    actionButton("bg_recompute", "Recompute the stored set"))
)

explore_tab <- tabPanel("Explore", fluidRow(style = "padding-top: 20px;",
  column(4, explore_controls),
  column(8,
    tableOutput("summary"),
    tags$p(tags$b("Network"), " (fill = attitude Y, ring = declaration X)"), plotOutput("net_plot", height = "600px"),
    tags$p(tags$b("Prevalence of pluralistic ignorance and related shares"), style = "margin-top: 30px;"), plotOutput("decomp_plot", height = "460px"),
    tags$p(tags$b("Share believing they are in the local minority against the prevalence of pluralistic ignorance"), ", for this run and for a stored set of runs from every scenario version", style = "margin-top: 30px;"), plotOutput("square_plot", height = "540px"),
    tags$p(tags$b("Declarations X and attitudes Y over rounds"), style = "margin-top: 30px;"), plotOutput("traj_plot", height = "460px"),
    tags$p(tags$b("Population values for each replicate"), style = "margin-top: 30px;"), DTOutput("rep_table"),
    fluidRow(style = "margin-top: 20px;", column(6, downloadButton("download_run", "Download this run (zip)", style = "width: 100%"),
             hp("The first replicate as a census of the whole population, written in every data configuration (folder for_analysts) with the ground truth beside it (folder key).")),
             column(6, downloadButton("download_nodes_run", "nodes.csv, every agent of every replicate", style = "width: 100%"))))
))

corpus_controls <- wellPanel(
  actionButton("gen", "GENERATE", class = "btn-primary", width = "100%"),
  helpText("Builds the datasets with the settings below. Every dataset must show the pattern: more than half of the agents, and of the respondents, believe that their attitude is the minority among their neighbors. Runs that do not are discarded and counted as attempts: random-start runs in which conformity does not produce the pattern, private-change runs in which the old declarations give way, and well-connected-minority runs in which the central minority does not dominate enough neighborhoods.", style = "margin-bottom: 14px; color: #777;"),
  h5s("Datasets per scenario version"),
  hp("One dataset is one survey of one simulated population. Enter how many datasets to draw from each version; a version set to 0 is left out."),
  fluidRow(column(6, numericInput("per_S1", "Random start", 30, min = 0, step = 1)),
           column(6, numericInput("per_S3", "Private change", 30, min = 0, step = 1))),
  fluidRow(column(4, numericInput("per_S2mixed", "Well-connected minority, conformity to declarations", 30, min = 0, step = 1)),
           column(4, numericInput("per_S2clair", "Well-connected minority, conformity to true attitudes", 30, min = 0, step = 1)),
           column(4, numericInput("per_S2pure", "Well-connected minority, conformity off", 30, min = 0, step = 1))),
  radioButtons("cen_choice", "Centrality measure (used for every dataset):", c("degree", "eigenvector", "betweenness"), inline = TRUE),
  hp("How central an agent is when the minority attitude is placed on central agents. Degree: number of contacts. Eigenvector: central in proportion to the centrality of one's contacts. Betweenness: share of shortest paths between other agents that pass through the agent."),
  sl("respondents", "Respondents per dataset \\(n\\):", 20, 1000, 150, 10),
  hp("How many agents of each population are surveyed, chosen at random. If a population is smaller than this, everyone is surveyed."),
  sl("n_range", "Population size \\(N\\):", 50, 2000, c(200, 500), 10),
  hp("Number of agents in each simulated population, drawn at random from this range for every dataset. Move both ends together to fix it."),
  h5s("Data configurations"),
  hp("What the survey records about each respondent. Every dataset is written once per configuration you tick, in its own folder, with the same dataset and respondent identifiers throughout, so the analysts can be given the sparsest configuration first and the others later. The Guide has the full table."),
  checkboxGroupInput("configs", NULL, choices = setNames(CONFIG_CODES, CONFIG_NAMES), selected = CONFIG_CODES),
  checkboxInput("add_pn", HTML("Add the item \\(P_{\\mathcal{N}_i}\\): the respondent's estimate of the share of their own contacts holding attitude 1 (column perceived_contacts_mean)"), FALSE),
  hp("One extra survey question, available in every configuration. In this model respondents extrapolate from their contacts, so \\(P_{\\mathcal{N}_i} = P_i\\) unless extrapolation noise is added under the parameter ranges."),
  numericInput("corpus_seed", "Seed:", 1, min = 0, step = 1),
  hp("Fixes the random numbers, so that the same settings and seed give exactly the same corpus."),
  checkboxInput("corpus_advanced", "Show parameter ranges", FALSE),
  conditionalPanel("input.corpus_advanced",
    hp("Each dataset draws its own value from these ranges, uniformly at random. A range with equal ends fixes the value."),
    sl("prev_range", "Share of the population holding the majority attitude:", 0.5, 0.99, c(0.55, 0.85), 0.01),
    hp("For the random start and the well-connected minority this is the share holding attitude 1. For the private change it is the share of agents who change their mind, which becomes the majority share after the change."),
    sl("ca_range", "Conformity \\(c_i \\sim \\mathrm{beta}(a, 1)\\), range of \\(a\\):", 0.5, 16, c(3, 12), 0.5),
    hp("One range for every scenario. Each agent's conformity parameter is drawn from beta(a, 1), whose mean is a / (a + 1): a = 3 gives a mean of 0.75, and larger a puts more agents near 1."),
    sl("lambda_range", "Strength of the link between centrality and the minority attitude \\(\\lambda\\):", 0, 12, c(2, 8), 0.5),
    hp("Agents receive the minority attitude with probability proportional to their centrality raised to the power lambda. At 0 every agent is equally likely; at 2 an agent twice as central is four times as likely; large values place the minority attitude on the most central agents almost surely."),
    h5s("Survey"),
    sl("noise_sd", "Extrapolation noise: standard deviation of \\(P_i - P_{\\mathcal{N}_i}\\):", 0, 0.3, 0, 0.01),
    hp("Respondents estimate the population share \\(P_i\\) from their contacts. With 0 it equals \\(P_{\\mathcal{N}_i}\\); with noise it departs from it at random, within 0 and 1. The item \\(P_{\\mathcal{N}_i}\\) is never noisy."),
    sl("alter_response", "Share of named contacts who answer the survey (configuration C4):", 0, 1, 1, 0.05),
    hp("A contact who is also a respondent always answers. Other contacts answer with this probability; the own answers \\((Y_j, d_j)\\) of a contact who did not answer are left empty. The census configuration C5 is unaffected."),
    h5s("Network family"),
    radioButtons("net_family", NULL, c("Preferential attachment (Barabási-Albert)" = "ba", "Random graph (Erdős-Rényi)" = "er", "Small world (Watts-Strogatz)" = "small_world")),
    hp("One family is used for every dataset in the corpus. Preferential attachment: agents are added one at a time and each new agent links to a few existing agents, chosen in proportion to how many contacts they already have, so a few agents become very well connected. Random graph: every pair of agents is linked independently with the same probability, so most agents have a similar number of contacts. Small world: agents sit on a ring, each linked to its nearest neighbors, and a share of the ties is rewired to agents chosen at random, which keeps local clustering while shortening paths. The friendship-paradox effect needs some agents to have far more contacts than others, which only preferential attachment provides: with the random graph and the small world, scenario 3 does not show the pattern and its datasets cannot be generated, and the random start rarely does. Choose those two families for the private change of mind, or for the random start with a large number of attempts."),
    conditionalPanel("input.net_family == 'ba'",
      checkboxGroupInput("m_choices", "Contacts made by each new agent \\(m\\) (one value is drawn per dataset):", 1:5, selected = 1:3, inline = TRUE),
      hp("The average number of contacts is about 2m; the earliest agents become the most connected.")),
    conditionalPanel("input.net_family == 'er'",
      sl("er_range", "Average number of contacts:", 1, 30, c(4, 8), 0.5),
      hp("Drawn per dataset from this range; every pair is then linked with the probability that gives this average.")),
    conditionalPanel("input.net_family == 'small_world'",
      checkboxGroupInput("ws_k", "Contacts per agent on the ring \\(k\\) (one value is drawn per dataset):", c(2, 4, 6, 8, 10), selected = c(4, 6, 8), inline = TRUE),
      sl("ws_beta", "Share of ties rewired \\(\\beta\\):", 0, 1, c(0.05, 0.3), 0.01),
      hp("Drawn per dataset from this range. At 0 the ring is untouched; at 1 every tie is random.")))
)

corpus_tab <- tabPanel("Generate corpus", fluidRow(style = "padding-top: 20px;",
  column(4, corpus_controls),
  column(8,
    verbatimTextOutput("corpus_status"),
    fluidRow(column(6, downloadButton("download_analysts", "Download the datasets for the analysts (zip)", class = "btn-primary", style = "width: 100%"),
                    hp("One folder per configuration, each with its README and CSV files, plus an overview README.")),
             column(6, downloadButton("download_key", "Download the key (zip)", style = "width: 100%"),
                    hp("key.csv, nodes.csv, edges.csv, corpus.json and a README. Keep it apart from the analysts."))),
    fluidRow(column(8, radioButtons("one_config", "Download a single configuration:", choices = setNames(CONFIG_CODES, CONFIG_NAMES), selected = "C1", width = "100%")),
             column(4, downloadButton("download_one_config", "Download this configuration only (zip)", style = "width: 100%; margin-top: 25px;"))),
    tags$p(tags$b("Key"), " (one row per dataset)", style = "margin-top: 20px;"), DTOutput("key_table"),
    tags$p(tags$b("Generated datasets: share believing they are in the local minority against the prevalence of pluralistic ignorance"), style = "margin-top: 30px;"), plotOutput("corpus_square", height = "520px"))
))

guide_tab <- tabPanel("Guide", fluidRow(style = "padding-top: 20px;", column(11, guide_html)))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 3, bootswatch = "paper"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;} pre.shiny-text-output {white-space: pre-wrap;} .table-summary td, .table-summary th {padding: 4px 14px;} .container-fluid {padding: 5pt 15pt 15pt 15pt;} .btn {margin-bottom: 10pt;}"),
  tags$head(
    tags$script(HTML("window.MathJax = { tex: { inlineMath: [['\\\\(', '\\\\)']], displayMath: [['\\\\[', '\\\\]']] }, svg: { fontCache: 'global' } };")),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js", async = NA)),
  titlePanel("Social Networks and Pluralistic Ignorance"),
  description_band,
  tabsetPanel(type = "tabs", corpus_tab, explore_tab, guide_tab)
)

# ------------------------------------------------------------------ server

params_from_input <- function(input) {
  p <- default_params()
  p$scenario <- input$scenario; p$pure <- input$scenario == "S2" && input$s2_mode == "off"
  p$perception <- if (input$scenario == "S2" && input$s2_mode == "attitudes") "attitudes" else "declarations"
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
  f <- file.path(cache_dir, sprintf("bg_cloud_v5_n%d.rds", n_bg))
  if (file.exists(f)) return(readRDS(f))
  specs <- list()
  for (s in 1:25) specs[[length(specs) + 1]] <- list(scenario = "S1", n = n_bg, topology = "ba", m = 2, pi0 = 0.6, seed = s)
  for (s in 1:25) specs[[length(specs) + 1]] <- list(scenario = "S1", n = n_bg, topology = "ba", m = 2, pi0 = 0.6, c_dist = "beta", c_a = 6, c_b = 1, seed = 100 + s)
  for (lam in c(0, 1, 2, 4, 8)) for (s in 1:5) {
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = TRUE, seed = 2000 + 10 * lam + s)
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = FALSE, c_dist = "beta", c_a = 3, c_b = 1, seed = 6000 + 10 * lam + s)
    specs[[length(specs) + 1]] <- list(scenario = "S2", n = n_bg, topology = "ba", m = 2, pi0 = 0.7, lambda = lam, pure = FALSE, perception = "attitudes", c_dist = "beta", c_a = 3, c_b = 1, seed = 7000 + 10 * lam + s)
  }
  for (psi in c(0.55, 0.65, 0.75, 0.85)) for (s in 1:6) specs[[length(specs) + 1]] <- list(scenario = "S3", n = n_bg, topology = "ba", m = 2, psi = psi, c_dist = "beta", c_a = 3, c_b = 1, seed = 9000 + round(100 * psi) + s)
  rows <- lapply(seq_along(specs), function(i) {
    if (!is.null(progress)) progress(i, length(specs))
    st <- run_scenario(specs[[i]]); gt <- ground_truth(st)
    data.frame(class = scenario_label(st$params), apparent = gt$apparent_share, prevalence = gt$pi_prevalence)
  })
  out <- do.call(rbind, rows); saveRDS(out, f); out
}

square_plot <- function(bg, pts = NULL, pts_label = "this run") {
  g <- ggplot() +
    geom_hline(yintercept = 0, color = PI_COL$grey_light) + geom_vline(xintercept = 0.5, linetype = 2, color = PI_COL$grey_light) +
    labs(x = "Share of agents who believe their attitude is the minority among their neighbors", y = "Prevalence of pluralistic ignorance") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + theme_pi() + theme(legend.text = element_text(size = 12))
  if (!is.null(bg)) {
    bg$class <- factor(bg$class, levels = names(CLASS_NAMES), labels = unname(CLASS_NAMES))
    g <- g + geom_point(data = bg, aes(apparent, prevalence, color = class), alpha = 0.45, size = 2.2) +
      scale_color_manual(values = setNames(unname(PI_CLASS_COL[names(CLASS_NAMES)]), unname(CLASS_NAMES)), drop = FALSE) +
      guides(color = guide_legend(ncol = 1))
  }
  if (!is.null(pts) && nrow(pts)) g <- g + geom_point(data = pts, aes(apparent, prevalence), shape = 21, size = 5, fill = PI_COL$orange, color = "white", stroke = 1.2) +
    annotate("text", x = pts$apparent[1], y = pts$prevalence[1], label = pts_label, hjust = -0.15, vjust = -0.7, size = 4.5)
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
      data.frame(scenario = scenario_name(x$st$params), seed = x$st$params$seed, mu_Y = gt$mu_Y, believe_local_minority = gt$apparent_share,
                 pattern_present = gt$pattern_present, pi_prevalence = gt$pi_prevalence, pi_prevalence_attitude1 = gt$pi_prevalence_a1, pi_prevalence_attitude0 = gt$pi_prevalence_a0,
                 wrong_about_local_majority = gt$perception_error_gross, perception_error = gt$perception_error, exposure_bias = gt$exposure_bias, Delta_F = gt$Delta_F, falsified_share = gt$falsified_share,
                 eta = gt$eta, converged = gt$converged, rounds = gt$rounds,
                 mean_degree = gt$mean_degree, degree_assortativity = gt$degree_assortativity, edge_homophily = gt$edge_homophily, n_internalized = gt$n_internalized)
    }))
  })
  output$summary <- renderTable({
    gt <- first()$gt
    data.frame(check.names = FALSE,
      "Share holding attitude 1 (mu_Y)" = fmt(gt$mu_Y),
      "Share who believe their attitude is the minority among their neighbors" = fmt(gt$apparent_share),
      "Share declaring against their attitude" = fmt(gt$falsified_share),
      "Prevalence of pluralistic ignorance" = fmt(gt$pi_prevalence),
      "Among holders of attitude 1" = fmt(gt$pi_prevalence_a1), "Among holders of attitude 0" = fmt(gt$pi_prevalence_a0),
      "Convergence" = sprintf("%s, %d rounds", if (gt$converged) "converged" else "NOT converged", gt$rounds))
  }, align = "c", spacing = "s", width = "100%", class = "table-summary")

  output$net_plot <- renderPlot({
    req(isTRUE(session$clientData$output_net_plot_width > 0), isTRUE(session$clientData$output_net_plot_height > 0))
    st <- first()$st; n <- st$g$n
    if (n > input$draw_max_n) {
      d <- data.frame(degree = st$g$deg, attitude = factor(st$Y, levels = c(1, 0), labels = c("attitude 1", "attitude 0")))
      return(ggplot(d, aes(degree, fill = attitude)) + geom_histogram(binwidth = 1, position = "stack") +
               scale_fill_manual(values = c("attitude 1" = PI_COL$blue, "attitude 0" = PI_COL$orange)) +
               labs(x = "Number of contacts", y = "Agents", subtitle = sprintf("n = %d exceeds the drawing cap; degree histogram by attitude", n)) + theme_pi())
    }
    g <- graph_from_edgelist(st$g$edges, directed = FALSE)
    set.seed(1); lay <- layout_with_fr(g)
    par(mar = c(0, 0, 0, 0), family = "sans")
    col1 <- PI_COL$blue; col0 <- "#EE4000"
    plot(g, layout = lay, vertex.size = max(3, min(12, 180 / sqrt(n))), vertex.label = NA,
         vertex.color = ifelse(st$Y == 1, col1, col0), vertex.frame.color = ifelse(st$X == 1, col1, col0),
         vertex.frame.width = 3, edge.color = "grey85", edge.width = 0.6)
    legend("topleft", bty = "n", pch = 21, pt.cex = 2.2, pt.bg = c(col1, col0, col1, col0), col = c(col1, col0, col0, col1), pt.lwd = 3, cex = 1.05, text.col = "#666666",
           legend = c("attitude 1, declares 1", "attitude 0, declares 0", "attitude 1, declares 0", "attitude 0, declares 1"))
  })
  output$decomp_plot <- renderPlot({
    req(isTRUE(session$clientData$output_decomp_plot_width > 0), isTRUE(session$clientData$output_decomp_plot_height > 0))
    gt <- first()$gt; st <- first()$st
    lev <- c("Believe own attitude is\nthe local minority", "Wrong about which attitude\nmost neighbors hold", "Declare against\ntheir attitude", "Pluralistic ignorance:\nall agents", "Pluralistic ignorance:\nholders of 1", "Pluralistic ignorance:\nholders of 0")
    perc <- if (identical(st$params$perception, "attitudes")) st$Y else st$X
    wrong_majority <- mean(vapply(seq_len(st$g$n), function(i) { nb <- st$g$nbrs[[i]]; if (!length(nb)) return(NA); (mean(perc[nb] == st$Y[i]) < 0.5) != (mean(st$Y[nb] == st$Y[i]) < 0.5) }, logical(1)), na.rm = TRUE)
    d <- data.frame(term = factor(lev, levels = lev), value = c(gt$apparent_share, wrong_majority, gt$falsified_share, gt$pi_prevalence, gt$pi_prevalence_a1, gt$pi_prevalence_a0))
    ggplot(d, aes(term, value, fill = term)) + geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = fmt(value)), vjust = -0.5, size = 5, color = "#444444") +
      scale_fill_manual(values = c(PI_COL$grey, PI_COL$grey, PI_COL$grey, PI_COL$purple, PI_COL$blue, "#EE4000")) + scale_y_continuous(limits = c(0, 1.08), expand = expansion(mult = c(0, 0))) +
      labs(x = NULL, y = "Share of agents") + theme_pi() + theme(axis.text.x = element_text(size = 11))
  })
  bg <- reactiveVal(NULL)
  observeEvent(input$bg_recompute, {
    f <- file.path(cache_dir, sprintf("bg_cloud_v5_n%d.rds", as.integer(input$bg_n))); if (file.exists(f)) file.remove(f)
    withProgress(message = "Computing the stored set of runs", value = 0, { bg(background_cloud(as.integer(input$bg_n), function(i, t) setProgress(i / t))) })
  })
  output$square_plot <- renderPlot({
    req(isTRUE(session$clientData$output_square_plot_width > 0), isTRUE(session$clientData$output_square_plot_height > 0))
    req(runs())
    if (is.null(bg())) withProgress(message = "Computing the stored set of runs (kept for later runs)", value = 0, { bg(background_cloud(as.integer(input$bg_n), function(i, t) setProgress(i / t))) })
    tb <- gt_table()
    square_plot(bg(), data.frame(apparent = tb$believe_local_minority, prevalence = tb$pi_prevalence), pts_label = sprintf("this run (%d replicate%s)", nrow(tb), if (nrow(tb) > 1) "s" else ""))
  })
  output$traj_plot <- renderPlot({
    req(isTRUE(session$clientData$output_traj_plot_width > 0), isTRUE(session$clientData$output_traj_plot_height > 0))
    st <- first()$st; tr <- st$trajectory; req(tr)
    d <- rbind(data.frame(round = tr$round, value = tr$mean_declaration, series = "Declarations (share declaring 1)"),
               data.frame(round = tr$round, value = tr$mean_attitude, series = "Attitudes (share holding 1)"))
    d <- rbind(data.frame(round = 0, value = c(mean(st$X0), mean(st$Y0)), series = unique(d$series)), d)
    outcome <- if (st$params$scenario == "S3") sprintf("Outcome: %s.", if (mean(st$X) < 0.5) "most declarations changed to the new attitude" else if (first()$gt$pattern_present) "most declarations remained at the old attitude" else "declarations partly changed") else "Share declaring 1 and share holding 1, by round."
    ggplot(d, aes(round, value, color = series)) + geom_line(linewidth = 1.5) + geom_point(size = 2.5) +
      geom_hline(yintercept = 0.5, linetype = 2, color = PI_COL$grey) + scale_color_manual(values = c(PI_COL$blue, PI_COL$black)) + ylim(0, 1) +
      labs(x = "Round", y = "Share", subtitle = outcome) + theme_pi()
  })
  output$rep_table <- renderDT({
    tb <- gt_table()[, c("scenario", "seed", "mu_Y", "believe_local_minority", "pattern_present", "pi_prevalence", "pi_prevalence_attitude1", "pi_prevalence_attitude0", "wrong_about_local_majority", "falsified_share", "perception_error", "exposure_bias", "Delta_F", "eta", "converged", "rounds", "mean_degree", "degree_assortativity", "edge_homophily", "n_internalized")]
    datatable(tb, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("mu_Y", "believe_local_minority", "pi_prevalence", "pi_prevalence_attitude1", "pi_prevalence_attitude0", "wrong_about_local_majority", "falsified_share", "perception_error", "exposure_bias", "Delta_F", "eta", "mean_degree", "degree_assortativity", "edge_homophily"), 3)
  })
  run_nodes <- reactive({ do.call(rbind, lapply(seq_along(runs()), function(i) {
    st <- runs()[[i]]$st; gt <- runs()[[i]]$gt
    perc <- if (identical(st$params$perception, "attitudes")) st$Y else st$X
    data.frame(replicate = i, seed = st$params$seed, node_id = seq_len(st$g$n), attitude = st$Y, declaration = st$X,
               perceived_prevalence = vapply(st$g$nbrs, function(nb) if (length(nb)) mean(perc[nb]) else NA_real_, numeric(1)),
               degree = st$g$deg, conformity = st$conformity, believes_own_attitude_is_local_minority = gt$apparent_individual, pluralistic_ignorance = gt$pi_individual) })) })
  run_edges <- reactive({ do.call(rbind, lapply(seq_along(runs()), function(i) { st <- runs()[[i]]$st; data.frame(replicate = i, seed = st$params$seed, node_a = st$g$edges[, 1], node_b = st$g$edges[, 2]) })) })
  output$download_nodes_run <- downloadHandler(filename = function() sprintf("nodes_%s_n%d_seed%d.csv", input$scenario, input$n, input$seed), content = function(file) write.csv(run_nodes(), file, row.names = FALSE))
  output$download_run <- downloadHandler(
    filename = function() sprintf("run_%s_n%d_seed%d.zip", input$scenario, input$n, input$seed),
    content = function(file) {
      st <- first()$st; sv <- survey(st, st$g$n, survey_seed = input$seed + 500000L)
      td <- file.path(tempdir(), sprintf("run_%s_n%d_seed%d", input$scenario, input$n, input$seed)); unlink(td, recursive = TRUE)
      write_run(st, sv, td, configs = CONFIG_CODES, add_pn = TRUE)
      make_zip(file, td)
    })

  # ---- generate corpus
  corpus <- reactiveVal(NULL); gen_error <- reactiveVal(NULL)
  observeEvent(input$gen, {
    counts <- c("S1" = as.integer(input$per_S1), "S3" = as.integer(input$per_S3), "S2-pure" = as.integer(input$per_S2pure), "S2-mixed" = as.integer(input$per_S2mixed), "S2-clairvoyant" = as.integer(input$per_S2clair))
    validate(need(sum(counts) > 0, "Ask for at least one dataset."))
    s <- default_corpus_settings()
    s$per_scenario <- counts
    validate(need(length(input$configs) > 0, "Tick at least one data configuration."))
    s$respondents <- as.integer(input$respondents); s$configs <- input$configs; s$add_pn <- isTRUE(input$add_pn); s$n_range <- as.integer(input$n_range)
    s$noise_sd <- input$noise_sd; s$alter_response_rate <- input$alter_response
    s$seed <- as.integer(input$corpus_seed); s$write_state <- TRUE
    s$prevalence_range <- input$prev_range; s$conformity_a_range <- input$ca_range
    s$lambda_range <- input$lambda_range; s$centrality <- input$cen_choice; s$ba_m <- as.integer(input$m_choices)
    s$network <- input$net_family; s$er_mean_degree <- input$er_range; s$ws_k <- as.integer(input$ws_k); s$ws_beta <- input$ws_beta
    out <- file.path(tempdir(), sprintf("blinded-%s-seed%d", format(Sys.time(), "%Y%m%d-%H%M%S"), s$seed)); dir.create(out, recursive = TRUE, showWarnings = FALSE)
    key <- tryCatch(withProgress(message = "Generating datasets", value = 0, {
      generate_blinded_corpus(s, out, progress = function(d, t, msg) setProgress(d / t, detail = msg))
    }), error = function(e) e)
    if (inherits(key, "error")) { corpus(NULL); gen_error(conditionMessage(key)); return() }
    gen_error(NULL)
    corpus(list(key = key, out = out, attempts = attr(key, "attempts"), configs = input$configs))
  })
  output$corpus_status <- renderText({
    cp <- corpus()
    if (!is.null(gen_error())) return(paste("No corpus was generated:", gen_error(), "With the random graph and the small world, scenario 3 never shows the pattern and the random start rarely does; use preferential attachment for those, or generate only the private change of mind."))
    if (is.null(cp)) return("")
    k <- cp$key; tab <- table(k$scenario_code); cnt <- function(x) if (x %in% names(tab)) tab[[x]] else 0L
    kept <- paste(sprintf("%s %d (%d)", CLASS_NAMES[SCENARIO_CODES], vapply(SCENARIO_CODES, cnt, integer(1)), cp$attempts[SCENARIO_CODES]), collapse = "; ")
    sprintf("%d datasets generated, written in %s. The analysts' package has one folder per configuration; the key is a separate download.\n\nDatasets kept (runs attempted): %s.\nAttitude coding reversed in %d datasets. The pattern is present in the sample of every dataset (%d of %d). Prevalence of pluralistic ignorance: mean %.3f, range %.3f to %.3f.",
            nrow(k), paste(cp$configs, collapse = ", "), kept, sum(k$attitude_coding_flipped), sum(k$pattern_in_sample), nrow(k), mean(k$pi_prevalence), min(k$pi_prevalence), max(k$pi_prevalence))
  })
  output$key_table <- renderDT({
    cp <- corpus(); req(cp)
    datatable(cp$key, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("prevalence_parameter", "mean_degree", "rewiring_share", "lambda", "majority_share", "apparent_share", "pi_prevalence", "pi_prevalence_attitude1", "pi_prevalence_attitude0", "perception_error", "exposure_bias", "perception_error_gross", "Delta_F", "falsified_share", "eta", "eta_sample_as_coded"), 3)
  })
  output$corpus_square <- renderPlot({
    req(isTRUE(session$clientData$output_corpus_square_width > 0), isTRUE(session$clientData$output_corpus_square_height > 0))
    cp <- corpus(); req(cp); k <- cp$key
    d <- data.frame(class = k$scenario_code, apparent = k$apparent_share, prevalence = k$pi_prevalence)
    square_plot(d) + labs(subtitle = "One point per dataset")
  })
  zip_download <- function(rel, name) downloadHandler(
    filename = function() { cp <- corpus(); if (is.null(cp)) name() else sprintf("%s_%s", basename(cp$out), name()) },
    content = function(file) { cp <- corpus(); req(cp); src <- file.path(cp$out, rel()); validate(need(dir.exists(src), "This configuration was not generated.")); make_zip(file, src) })
  output$download_analysts <- zip_download(function() "for_analysts", function() "for_analysts.zip")
  output$download_key <- zip_download(function() "key", function() "key.zip")
  output$download_one_config <- zip_download(function() file.path("for_analysts", input$one_config), function() sprintf("%s.zip", input$one_config))

}

shinyApp(ui, server)
