# SIR on Networks

Browser port of the NetLogo model `SIR-1.nlogo` (kept here as the source of
truth): susceptible, infectious, removed contagion on random (Erdős–Rényi),
small-world (Watts–Strogatz), complete, and connected-caveman networks.

`index.html` is self-contained (no build, no server; one Google Fonts link
with system fallbacks). Open it locally or serve it as a static page.

## The model, as ported

- Every agent starts susceptible. Infection enters through spontaneous
  infection (probability per tick that one random susceptible agent turns
  infectious), through the initially infectious agents (an addition; default
  0 as in the original), or by clicking a node.
- Each tick, the agents infectious at the start of the tick act in random
  order: advance the infectious clock; if it exceeds `PeriodsInfectious` and
  `PerpetuallyInfectious` is off, become removed; otherwise infect each
  susceptible neighbor independently with `ProbTransmission`. Agents infected
  this tick do not transmit until the next. Then the spontaneous infection
  draw, then `tick`.
- The run stops when no susceptible agent remains (the original's `stop`),
  and also when nothing can change (no infectious agents and spontaneous
  infection at 0; the original would spin).
- Networks follow the `nw` extension calls: `NetworkProb` is the link
  probability for Random and the rewiring probability for Small world;
  `NeighborhoodSize` is neighbors per side for Small world and clique size
  for Connected caveman (the last clique takes the remainder; one member of
  each clique links to the next in a ring). Directed links in the original
  are effectively undirected because `link-neighbors` ignores direction.
- Defaults match the original interface: n = 100, connected caveman,
  NeighborhoodSize 16, NetworkProb 0.01, ProbTransmission 0.03,
  Spontaneous 0.01, PeriodsInfectious 10, PerpetuallyInfectious off.

Additions beyond the original: seed (reproducible runs), speed control and
single step, click-to-infect, network statistics (links, mean degree, mean
local clustering coefficient, mean path length, components; the same
quantities the BehaviorSpace experiments record), stat tiles, a data table
with copy-as-CSV, light and dark themes.

## Deploy

- GitHub Pages: enable Pages for this repository (Settings, Pages, deploy
  from branch `main`, folder `/`). The app is then served at
  `https://amohseni.github.io/Social-Networks-and-Pluralistic-Ignorance/sir/`.
- Any static host: upload `index.html`.
