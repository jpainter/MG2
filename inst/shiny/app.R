# Magic Glasses 2 — Shiny Application
# Launch with: MG2::run_mg2()
#
# Entry point for the Shiny app bundled in inst/shiny/. The app itself is in
# app_main.R. It is evaluated in an environment whose parent holds every
# function in the MG2 namespace, so widgets can call internal (unexported)
# helpers by bare name, and nothing is written to the user's .GlobalEnv.
# Packages attached by app_main.R via library() are still found through the
# search path, which sits above that environment.

source("app_main.R", local = MG2:::.mg2_app_env(), encoding = "UTF-8")$value
