# MG2 demo deployment
#
# Connect Cloud: deploy via GitHub at connectcloud.posit.co
# shinyapps.io:  rsconnect::deployApp(appDir="deploy", appName="MG2-MagicGlasses2")

Sys.setenv(MG2_DEMO_MODE = "1")

library(MG2)
shiny::shinyAppDir(system.file("shiny", package = "MG2"))
