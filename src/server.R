library(shiny)

server <- function(input, output) {

    res <- dbSendQuery(conn, "SELECT * FROM equipment_access LIMIT 100000")
    f <- dbFetch(res)

    leaf <- leaflet() %>%
                addTiles() 

    observeEvent(input$map_background_bounds, {
        fetchDB(input)
  })

    data_df_orig <- data.frame(
        X = f$X,
        Y = f$Y,
        Label = f$pop
    ) %>% distinct()

    data_sf_orig <- st_as_sf(
        data_df_orig,
        coords = c("X", "Y"),
        crs = 3035
    )

    data_sf_4326 <- st_transform(data_sf_orig, 4326)

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

   
    print("DONE")
    
    
    output$map_background <- renderLeaflet({leaf})

}
