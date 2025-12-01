library(DBI)
library(RPostgres)

db_host <- Sys.getenv("DB_HOST")
db_port <- Sys.getenv("DB_PORT")
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")

tryCatch(
    {
        conn <- dbConnect(
            RPostgres::Postgres(),
            dbname = Sys.getenv("DB_NAME", "shiny_db"),
            host = Sys.getenv("DB_HOST", "127.0.0.1"),
            port = Sys.getenv("DB_PORT", 5432),
            user = Sys.getenv("DB_USER", "visualisation"),
            password = Sys.getenv("DB_PASSWORD", "visualisation")
        )
        message("Connected successfully")

        if (db_host != "localhost" || db_host != "127.0.0.1") {
            message("DB_HOST is not localhost, skipping data load")
            return()
        }

        is_empty <- function(con, table) {
            tryCatch(
                {
                    query <- sprintf("SELECT COUNT(*) AS n FROM %s;", table)
                    n <- dbGetQuery(con, query)$n
                    return(n == 0)
                },
                error = function(e) {
                    return(TRUE)
                }
            )
        }

        if (is_empty(conn, "equipment_access")) {
            message("Reading Parquet file...")
            df <- read_parquet("../data/donnees-2024-reg94.parquet")
            df_data_frame <- as.data.frame(df)
            message("Writing to Database...")
            dbWriteTable(conn, "equipment_access", df_data_frame, overwrite = TRUE, row.names = FALSE)
            message("Success! Data loaded.")
        } else {
            message("Database already loaded, skipping data load")
        }

        # dbDisconnect(conn)
    },
    error = function(e) {
        message("DB ERROR: ", e$message)
    }
)
