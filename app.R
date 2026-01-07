# install.packages("shiny", repos = "http://cran.us.r-project.org", dependencies = TRUE)
library(shiny)
library(leaflet)
library(bslib)
library(arrow)
library(geosphere)
library(sf)
library(dplyr)

source("global.R")

source("./src/ui.R")
source("./src/server.R")
shinyApp(ui, server)
