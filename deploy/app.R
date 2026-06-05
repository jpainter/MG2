# MG2 demo deployment — shinyapps.io entry point
#
# Deploy with:
#   rsconnect::deployApp(
#     appDir  = "deploy",
#     appName = "MG2"
#   )
#
# After deploying, set MG2_DEMO_MODE=1 in:
#   shinyapps.io dashboard → App → Settings → Environment Variables
# (or uncomment the Sys.setenv line below for a self-contained deploy)
#
# Sys.setenv(MG2_DEMO_MODE = "1")

library(MG2)
shiny::shinyAppDir(system.file("shiny", package = "MG2"))
