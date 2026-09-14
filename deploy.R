# Publish the app to shinyapps.io (account: amohseni). One-time setup, from
# https://www.shinyapps.io/admin/#/tokens (Show token, copy the command):
#   rsconnect::setAccountInfo(name = "amohseni", token = "...", secret = "...")
# Then, from the repo root:
#   Rscript deploy.R
# The app is served at https://amohseni.shinyapps.io/Social-Networks-and-Pluralistic-Ignorance/
# .rscignore keeps corpora, docs, tests and grids out of the bundle.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
rsconnect::deployApp(
  appDir = ".",
  appName = "Social-Networks-and-Pluralistic-Ignorance",
  account = "amohseni",
  appFiles = c("app.R", list.files("R", full.names = TRUE)),
  forceUpdate = TRUE
)
