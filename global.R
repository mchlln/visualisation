library(DBI)
library(RPostgres)

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
    is_empty <- function(con, table) {
        tryCatch({
            query <- sprintf("SELECT COUNT(*) AS n FROM %s;", table)
            n <- dbGetQuery(con, query)$n
            return(n == 0)}, 
            error = function(e){return (TRUE)})
        
    }

    if(is_empty(conn, "equipment_access")){
        message("Reading Parquet file...")
        df <- read_parquet("data/donnees-2024-reg94.parquet")
        df_data_frame <- as.data.frame(df)
        message("Writing to Database...")
        dbWriteTable(conn, "equipment_access", df_data_frame, overwrite = TRUE, row.names = FALSE)
        message("Success! Data loaded.")
    }else{
        message("Database already loaded, skipping data load")
    }
    
    res <- dbSendQuery(conn, "SELECT * FROM equipment_access LIMIT 5")
    dbFetch(res)
    print(res)


    #dbDisconnect(conn)

}, error = function(e) {
    message("DB ERROR: ", e$message)
})