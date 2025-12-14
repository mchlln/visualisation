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
    
    max_fetch <- input$slider

    res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" >= %.0f AND \"X\" <= %.0f AND \"Y\" >= %.0f AND \"Y\" <= %.0f LIMIT %.0f", x_min, x_max, y_min, y_max, max_fetch))
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

selectSquare <- function(input, plot_data_rv, selected_point_rv, table_data_rv){
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
  
  selected_point_rv(nearest_point)
  
  query <- sprintf("SELECT * FROM equipment_access WHERE \"X\" = %.0f AND \"Y\" = %.0f ORDER BY distance ASC", nearest_point$X, nearest_point$Y)
  res <- dbSendQuery(conn, query)
  tableData <- dbFetch(res)
  table_data_rv(tableData)
  
  # TODO: after merge call find square to link plots to user selection
  findSquare(data.frame(X=nearest_point$X, Y=nearest_point$Y), input, plot_data_rv)
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

findSquare<- function(point, input, plot_data_rv){
  eq <- input$selectEquimpent
  if(eq == "."){
    eq <- ""
  }
  equipment_type <- dbQuoteString(conn, paste0(eq, "%"))
  
  if(is.null(point) || nrow(point) == 0) {
    plot_data_rv(data.frame())
    return()
  }

  res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" = %.0f AND \"Y\" = %.0f AND \"typeeq_id\" LIKE %s",
                                   point$X, point$Y, equipment_type))
  plot_data_rv(dbFetch(res))
  dbClearResult(res)
}

server <- function(input, output) {

    plot_data <- reactiveVal(data.frame())
    table_data <- reactiveVal(data.frame())
    selected_point <- reactiveVal(NULL)

    observeEvent(input$map_background_click, {
      selectSquare(input, plot_data, selected_point, table_data)
    })


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
       if (!is.null(selected_point())) {
         findSquare(selected_point(), input, plot_data)
       }
    })
    
    output$table <- renderTable({
      d <- plot_data()
      req(nrow(d) > 0)
      d_ordered <- d[order(d$duree), ]
      code_signification <- setNames(legend$Libelle_TYPEQU, legend$TYPEQU)
      d_ordered$signification <- sapply(d_ordered$typeeq_id, function(x) code_signification[match(x, names(code_signification))])
      d_ordered[,c(15,5,7)]
    })

    output$distPlot <- renderPlot({
      f <- plot_data()
      req(nrow(f) > 0)
      
      h <- hist(f$duree, col = "#007bc2", border = "white",
                xlab = "Duration of travel from the square to an equipment (in minutes)",
                ylab = "Number of equipments",
                main = "Histogram of travel time to an equipment",
                xaxt = "n",
                yaxt = "n")
      axis(1, at = seq(0, max(f$duree, na.rm = TRUE)+20, by = 20))
      axis(2, at = seq(0,max(h$counts)+5, by = 5))
    })

   
    print("Server update done!")
    
    output$map_background <- renderLeaflet({leaf})

}
