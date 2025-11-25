library(shiny)
library(leaflet)
library(bslib)
library(DBI)
library(RPostgres)

source("./src/ui.R")
source("./src/server.R")
shinyApp(ui, server)
