library(shiny)
library(leaflet)
library(bslib)
library(DBI)
library(RPostgres)
library(arrow)

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
    message("Reading Parquet file...")
    df <- read_parquet("data/donnees-2024-reg94.parquet")
    df_data_frame <- as.data.frame(df)
    message("Writing to Database...")
    dbWriteTable(conn, "equipment_access", df_data_frame, overwrite = TRUE, row.names = FALSE)
    message("Success! Data loaded.")
    dbDisconnect(conn)

}, error = function(e) {
    message("DB ERROR: ", e$message)
})

source("./src/ui.R")
source("./src/server.R")
shinyApp(ui, server)
