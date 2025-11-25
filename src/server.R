library(shiny)

server <- function(input, output) {
    tryCatch({
        conn <- dbConnect(
            RPostgres::Postgres(),
            dbname = "shiny_db",
            host = "127.0.0.1",
            port = 5432,
            user = "visualisation",
            password = "visualisation"
        )
        message("Connected successfully")
    }, error = function(e) {
        message("DB ERROR: ", e$message)
    })
    output$map_background <- renderLeaflet({
            leaflet() %>%
                addTiles()
    })

}
