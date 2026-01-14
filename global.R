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

        stopifnot(!is_empty(conn, "equipment_access"))

        legend <- read.csv(file = "data/BPE24_table_passage.csv", sep = ";", header = T)

        culture <- read.csv(file = "data/depenses-culturelles-des-communes-total-2023-2023.csv", sep = ",", header = T)
        # print(culture)
        # print(legend)

        # dbDisconnect(conn)
    },
    error = function(e) {
        message("DB ERROR: ", e$message)
    }
)
