library(shiny)

server <- function(input, output) {
    
    output$map_background <- renderLeaflet({
            leaflet() %>%
                addTiles()
    })

}
