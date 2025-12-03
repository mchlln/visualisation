library(shiny)

fetchDB <- function(input) {
    north_lat <- input$map_background$north
    south_lat <- input$map_background$south
    leafletProxy("map_background") %>% clearMarkers() %>% clearShapes() 
    
    df <- data.frame(c(input$map_background_bounds$west, input$map_background_bounds$east), c(input$map_background_bounds$north, input$map_background_bounds$south))
    colnames(df) <- c("X", "Y")

    data_sf_orig <- st_as_sf(
        df,
        coords = c("X", "Y"),
        crs = 4326
    )

    data_sf_3035 <- st_transform(data_sf_orig, 3035)

    coords_3035 <- st_coordinates(data_sf_3035)
    x_min <- min(coords_3035[, 1])
    x_max <- max(coords_3035[, 1])
    y_min <- min(coords_3035[, 2])
    y_max <- max(coords_3035[, 2])

    res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" >= %.0f AND \"X\" <= %.0f AND \"Y\" >= %.0f AND \"Y\" <= %.0f LIMIT 10000", x_min, x_max, y_min, y_max))
    f <- dbFetch(res)

    if (nrow(f) == 0){
      print("No squared referenced in this area")
      return ()
    }

    data_sf_4326 <- dbCoordsToLeaflet(f)

    
    for (elt2 in seq_len(nrow(data_sf_4326))){
        elt <- data_sf_4326[elt2, ]

        #leafletProxy("map_background") %>% addMarkers(
        #lng = st_coordinates(elt)[,1],
        #lat = st_coordinates(elt)[,2],
        #label = elt$Label
        #)
        bottomRightPoint <- destPoint(st_coordinates(elt)[1,], 135, sqrt(2)*100)
        topLeftPoint <- destPoint(st_coordinates(elt)[1,], 315, sqrt(2)*100)
        leafletProxy("map_background") %>% addRectangles(
        lng1=topLeftPoint[1],
        lat1=topLeftPoint[2],
        lng2=bottomRightPoint[1],
        lat2=bottomRightPoint[2],
        color="green"
        )

    }
    
}


dbCoordsToLeaflet <- function(df){
    data_df_orig <- data.frame(
        X = df$X,
        Y = df$Y,
        Label = df$pop
    ) %>% distinct()

    data_sf_orig <- st_as_sf(
        data_df_orig,
        coords = c("X", "Y"),
        crs = 3035
    )

    data_sf_4326 <- st_transform(data_sf_orig, 4326)

    return (data_sf_4326)

}

findSquare<- function(point,input, output){
  eq <- input$selectEquimpent
  if(eq == "."){
    eq <- ""
  }
  equipment_type <- dbQuoteString(conn, paste0(eq, "%"))
  res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" = %.0f AND \"Y\" = %.0f AND \"typeeq_id\" LIKE %s",
                                   point$X, point$Y, equipment_type))
  f <- dbFetch(res)
  output$distPlot <- renderPlot({
    
   h <- hist(f$duree, col = "#007bc2", border = "white",
       xlab = "Duration of travel from the square to an equipment (in minutes)",
       ylab = "Number of equipments",
       main = "Histogram of travel time to an equipment",
       xaxt = "n",
       yaxt = "n")
  axis(1, at = seq(0, max(f$duree)+20, by = 20))
  axis(2, at = seq(0,max(h$counts)+5, by = 5))
  })
}

server <- function(input, output) {

    res <- dbSendQuery(conn, "SELECT * FROM equipment_access LIMIT 10")
    f <- dbFetch(res)
    print(f)

    leaf <- leaflet() %>%
                addTiles() 

    observeEvent(input$map_background_bounds, {
        fetchDB(input)
    })

    data_sf_4326 <- dbCoordsToLeaflet(f)
    
    for (elt2 in seq_len(nrow(data_sf_4326))){
        elt <- data_sf_4326[elt2, ]

        leaf <- addMarkers(
        map = leaf,
        lng = st_coordinates(elt)[,1],
        lat = st_coordinates(elt)[,2],
        label = elt$Label
        )
        bottomRightPoint <- destPoint(st_coordinates(elt)[1,], 135, sqrt(2)*100)
        topLeftPoint <- destPoint(st_coordinates(elt)[1,], 315, sqrt(2)*100)
        leaf <- addRectangles(
        map = leaf,
        lng1=topLeftPoint[1],
        lat1=topLeftPoint[2],
        lng2=bottomRightPoint[1],
        lat2=bottomRightPoint[2],
        color="green"
        )

    }
    observeEvent(input$selectEquimpent, {
      findSquare(data.frame(X=4205400, Y=2119200), input, output)
    })


   
    print("Server update done!")
    
    
    output$map_background <- renderLeaflet({leaf})

}
