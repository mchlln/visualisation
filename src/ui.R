library(shiny)

ui <- fluidPage(

    titlePanel("Visualisation 2025"),
    sliderInput("slider", label = "max number of areas", min = 10000, max = 5000000, value = 10000),
    leafletOutput("map_background"),
    selectInput("selectEquimpent", 
                label="Select Equipment category",
                choices = list("GLOBAL" = ".",
                               "SERVICES POUR LES PARTICULIERS " = "A",
                               "COMMERCES" = "B", 
                               "ENSEIGNEMENT" = "C", 
                               "SANTÉ ET ACTION SOCIALE" = "D", 
                               "TRANSPORTS ET DÉPLACEMENTS " ="E", 
                               "SPORTS, LOISIRS ET CULTURE "="F",
                               "TOURISME"="G"),
                selected = "."
    ),
    plotOutput(outputId = "distPlot"),
)
