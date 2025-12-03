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

    res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" >= %.0f AND \"X\" <= %.0f AND \"Y\" >= %.0f AND \"Y\" <= %.0f LIMIT 100000", x_min, x_max, y_min, y_max))
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
        color="green",
        label = elt$Label
        )

    }
}

selectSquare <- function(input){
  click_pos <-input$map_background_click
  
  df <- data.frame(click_pos$lng, click_pos$lat)
  colnames(df) <- c("X", "Y")
  
  data_sf_orig <- st_as_sf(
    df,
    coords = c("X", "Y"),
    crs = 4326
  )
  
  data_sf_3035 <- st_transform(data_sf_orig, 3035)
  
  coords_3035 <- st_coordinates(data_sf_3035)
  
  x<-coords_3035[1, 1]
  y<-coords_3035[1, 2]
  
  x_range_min <- x - 100
  x_range_max <- x + 100
  y_range_min <- y - 100
  y_range_max <- y + 100
  
  query <- sprintf("
  SELECT *, 
         SQRT(POW(\"X\" - %.0f, 2) + POW(\"Y\" - %.0f, 2)) AS calculated_distance
  FROM equipment_access
  WHERE \"X\" BETWEEN %.0f AND %.0f AND \"Y\" BETWEEN %.0f AND %.0f
  ORDER BY calculated_distance ASC
  LIMIT 1", 
  x, y, x_range_min, x_range_max, y_range_min, y_range_max)
  
  

  res <- dbSendQuery(conn, query)
  nearest_point <- dbFetch(res)
  
  if (nrow(nearest_point) == 0){
    print("No data for given area")
    leafletProxy("map_background") %>% clearMarkers()
    return ()
  }
  
  # TODO: after merge call find square to link plots to user selection
  
  data_sf_4326 <- dbCoordsToLeaflet(nearest_point)
  leafletProxy("map_background") %>% clearMarkers() %>% addMarkers(
    lng = st_coordinates(data_sf_4326)[1,1],
    lat = st_coordinates(data_sf_4326)[1,2],
  )
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

server <- function(input, output) {

    res <- dbSendQuery(conn, "SELECT * FROM equipment_access LIMIT 10")
    f <- dbFetch(res)

    leaf <- leaflet() %>%
                addTiles() 

    observeEvent(input$map_background_bounds, {
        fetchDB(input)
    })
    
    observeEvent(input$map_background_click, {
      selectSquare(input)
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

   
    print("Server update done!")
    
    
    output$map_background <- renderLeaflet({leaf})

}
