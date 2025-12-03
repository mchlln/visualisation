library(shiny)

ui <- fluidPage(

    titlePanel("Visualisation 2025"),
    sliderInput("slider", label = "max number of areas", min = 10000, max = 5000000, value = 10000),
    leafletOutput("map_background"),
)
