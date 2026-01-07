library(shiny)

fetchDB <- function(input) {
  north_lat <- input$map_background$north
  south_lat <- input$map_background$south
  leafletProxy("map_background") %>%
    clearMarkers() %>%
    clearShapes()

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

  incProgress(0.2, detail = "Querying Database")
  res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" >= %.0f AND \"X\" <= %.0f AND \"Y\" >= %.0f AND \"Y\" <= %.0f LIMIT %.0f", x_min, x_max, y_min, y_max, max_fetch))

  incProgress(0.4, detail = "Processing Data")
  f <- dbFetch(res)

  if (nrow(f) == 0) {
    print("No squared referenced in this area")
    return()
  }

  data_sf_4326 <- dbCoordsToLeaflet(f)


  incProgress(0.3, detail = "Rendering Map")
  for (elt2 in seq_len(nrow(data_sf_4326))) {
    elt <- data_sf_4326[elt2, ]

    # leafletProxy("map_background") %>% addMarkers(
    # lng = st_coordinates(elt)[,1],
    # lat = st_coordinates(elt)[,2],
    # label = elt$Label
    # )
    bottomRightPoint <- destPoint(st_coordinates(elt)[1, ], 135, sqrt(2) * 100)
    topLeftPoint <- destPoint(st_coordinates(elt)[1, ], 315, sqrt(2) * 100)
    leafletProxy("map_background") %>% addRectangles(
      lng1 = topLeftPoint[1],
      lat1 = topLeftPoint[2],
      lng2 = bottomRightPoint[1],
      lat2 = bottomRightPoint[2],
      color = "green",
      label = elt$Label
    )
  }
}

selectSquare <- function(input, plot_data_rv, selected_point_rv, table_data_rv) {
  click_pos <- input$map_background_click

  df <- data.frame(click_pos$lng, click_pos$lat)
  colnames(df) <- c("X", "Y")

  data_sf_orig <- st_as_sf(
    df,
    coords = c("X", "Y"),
    crs = 4326
  )

  data_sf_3035 <- st_transform(data_sf_orig, 3035)

  coords_3035 <- st_coordinates(data_sf_3035)

  x <- coords_3035[1, 1]
  y <- coords_3035[1, 2]

  x_range_min <- x - 100
  x_range_max <- x + 100
  y_range_min <- y - 100
  y_range_max <- y + 100

  query <- sprintf(
    "
  SELECT *,
         SQRT(POW(\"X\" - %.0f, 2) + POW(\"Y\" - %.0f, 2)) AS calculated_distance
  FROM equipment_access
  WHERE \"X\" BETWEEN %.0f AND %.0f AND \"Y\" BETWEEN %.0f AND %.0f
  ORDER BY calculated_distance ASC
  LIMIT 1",
    x, y, x_range_min, x_range_max, y_range_min, y_range_max
  )


  res <- dbSendQuery(conn, query)
  nearest_point <- dbFetch(res)

  if (nrow(nearest_point) == 0) {
    print("No data for given area")
    leafletProxy("map_background") %>% clearMarkers()
    return()
  }

  selected_point_rv(nearest_point)

  query <- sprintf("SELECT * FROM equipment_access WHERE \"X\" = %.0f AND \"Y\" = %.0f ORDER BY distance ASC", nearest_point$X, nearest_point$Y)
  res <- dbSendQuery(conn, query)
  tableData <- dbFetch(res)
  table_data_rv(tableData)

  # TODO: after merge call find square to link plots to user selection
  findSquare(data.frame(X = nearest_point$X, Y = nearest_point$Y), input, plot_data_rv)
  data_sf_4326 <- dbCoordsToLeaflet(nearest_point)
  leafletProxy("map_background") %>%
    clearMarkers() %>%
    addMarkers(
      lng = st_coordinates(data_sf_4326)[1, 1],
      lat = st_coordinates(data_sf_4326)[1, 2],
    )
}


dbCoordsToLeaflet <- function(df) {
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

  return(data_sf_4326)
}

findSquare <- function(point, input, plot_data_rv) {
  eq <- input$selectEquimpent
  if (eq == ".") {
    eq <- ""
  }
  equipment_type <- dbQuoteString(conn, paste0(eq, "%"))

  if (is.null(point) || nrow(point) == 0) {
    plot_data_rv(data.frame())
    return()
  }

  res <- dbSendQuery(conn, sprintf(
    "SELECT * FROM equipment_access WHERE \"X\" = %.0f AND \"Y\" = %.0f AND \"typeeq_id\" LIKE %s",
    point$X, point$Y, equipment_type
  ))
  plot_data_rv(dbFetch(res))
  dbClearResult(res)
}

# Creates a list containing the number of inhabitants and bugdet per inhabitant in all cities present in the culture dataset
getNbInhabitant <- function(input,inCultureDataset) {
  query <- dbSendQuery(conn, sprintf("SELECT DISTINCT depcom FROM equipment_access"))
  cities <- dbFetch(query)
  dbClearResult(query)
  if(inCultureDataset == TRUE){
    inhabitantsFrance <- data.frame(City = culture$Code_Insee, Inhabitants = culture$Population.2023, BudgetPerInhabitant = culture$Depenses_culturelles_totales.euros.par.habitant, GlobalBudget = culture$Depenses_culturelles_totales..K..)
  }else{
    inhabitantsFrance <- data.frame(City = towns$CODGEO, Inhabitants = towns$Population)
  }
  
  inhabitantsDataset <- inhabitantsFrance[inhabitantsFrance$City %in% cities$depcom, ]
  return(inhabitantsDataset)
}

# Create a dataset containing the mean distance and time to a cultural equimpment from each city present in getNbInhabitant
computeDistTimeToCulture <- function(input,inCultureDataset) {
  infoCity <- getNbInhabitant(input,inCultureDataset)
  city_list <- paste0("'", infoCity$City, "'", collapse = ", ")
  city_list_sql <- sprintf("(%s)", city_list)
  query <- dbSendQuery(conn, sprintf("SELECT depcom, typeeq_id, distance, duree FROM equipment_access WHERE depcom IN %s AND typeeq_id LIKE 'F3%%'", city_list_sql))
  dataCitites <- dbFetch(query)
  dbClearResult(query)

  if (nrow(dataCitites) == 0) {
    return(data.frame())
  }

  res_agg <- aggregate(list(mean_distance = dataCitites$distance, mean_time = dataCitites$duree),
    by = list(City = dataCitites$depcom, eq_culture = dataCitites$typeeq_id),
    FUN = mean
  )

  totalInfo <- merge(res_agg, infoCity, by = "City")
  totalInfo$is_close <- as.integer(totalInfo$mean_time < 10)
  if(inCultureDataset == TRUE){
    colnames(totalInfo) <- c("City", "eq_culture", "mean_distance", "mean_time", "population", "budget_per_inhabitant", "global_budget", "is_close")
  }else{
    colnames(totalInfo) <- c("City", "eq_culture", "mean_distance", "mean_time", "population", "is_close")
  }

  return(totalInfo)
}

# Create a dataset containing the mean distance to an equipment depending on the size of the population of a city
computeTimeToCultureForPopSize <- function(input,inCultureDataset ) {
timeToCulture <- computeDistTimeToCulture(input,inCultureDataset)

  if (nrow(timeToCulture) == 0) {
    return(data.frame())
  }

  popSize_labels <- c("1-99", "100-999", "1000-9999", "10000-99999", "100000+")

  timeToCulture$pop_size_category <- cut(timeToCulture$population,
    breaks = c(0, 99, 999, 9999, 99999, Inf),
    labels = popSize_labels,
    right = TRUE
  )

  # Aggregate mean time by both population size category and equipment type
  result <- aggregate(list(mean_time = timeToCulture$mean_time),
    by = list(
      pop_size_category = timeToCulture$pop_size_category,
      eq_culture = timeToCulture$eq_culture
    ),
    FUN = mean
  )

  return(result)
}

createPlotCloseEqToBudget <- function(input, column){
  culturalData <- computeDistTimeToCulture(input, TRUE)
  req(nrow(culturalData) > 0)

  city_data <- aggregate(list(is_close = culturalData$is_close),
    by = list(City = culturalData$City),
    FUN = sum
  )

  city_budget <- unique(culturalData[, c("City", column)])
  print(city_budget)

  plot_culture <- merge(city_data, city_budget, by = "City")
  print(plot_culture)
  if(column == "budget_per_inhabitant"){
    lab = "Budget par Habitant"
  }else{
    lab = "Budget Global"
  }
  print("plotculture$column")
  print(plot_culture[,3])
  return (plot(
    x = plot_culture[,3], y = plot_culture$is_close,
    xlab = lab,
    ylab = "Nombre de type d'équipement à moins de 10 min",
    main = "Budget Par Habitant Alloué à la Culture vs. Accès à un équipement culturel",
    pch = 19, col = "#007bc2"
  ))
}

createBarplotCulturalEq <- function(input){
    data <- computeTimeToCultureForPopSize(input, FALSE)
    req(nrow(data) > 0)

    # Get unique equipment types and assign colors
    equipment_types <- unique(data$eq_culture)
    colors <- terrain.colors(length(equipment_types))
    equipment_colors <- setNames(colors, equipment_types)

    # Get equipment labels from legend
    code_signification <- setNames(legend$Libelle_TYPEQU, legend$TYPEQU)

    # Convert data to wide format for grouped barplot
    plot_data <- data
    plot_data$eq_label <- sapply(
      plot_data$eq_culture,
      function(x) code_signification[match(x, names(code_signification))]
    )

    # Create matrix for barplot
    pop_categories <- unique(plot_data$pop_size_category)
    barplot_matrix <- matrix(NA, nrow = length(equipment_types), ncol = length(pop_categories))
    rownames(barplot_matrix) <- sapply(equipment_types, function(x) code_signification[match(x, names(code_signification))])
    colnames(barplot_matrix) <- pop_categories

    for (i in seq_along(equipment_types)) {
      for (j in seq_along(pop_categories)) {
        subset_data <- plot_data[plot_data$eq_culture == equipment_types[i] &
          plot_data$pop_size_category == pop_categories[j], ]
        if (nrow(subset_data) > 0) {
          barplot_matrix[i, j] <- subset_data$mean_time[1]
        }
      }
    }

    # Create grouped barplot
    return (barplot(barplot_matrix,
      beside = TRUE,
      col = colors,
      xlab = "Population",
      ylab = "Temps moyen de trajet (en minutes)",
      main = "Temps de trajet moyen vers un équipement culturel en fonction de la taille de la population",
      legend.text = rownames(barplot_matrix),
      args.legend = list(x = "topright"))
    )
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
    withProgress(message = "Loading data...", value = 0, {
      fetchDB(input)
    })
  })

  observeEvent(input$color_map_bounds, {
    withProgress(message = "Loading heatmap...", value = 0, {
      fetchDBColor(input)
    })
  })

  data_sf_4326 <- dbCoordsToLeaflet(f)

  color_map <- leaflet() %>%
    addTiles()

  for (elt2 in seq_len(nrow(data_sf_4326))) {
    elt <- data_sf_4326[elt2, ]

    leaf <- addMarkers(
      map = leaf,
      lng = st_coordinates(elt)[, 1],
      lat = st_coordinates(elt)[, 2],
      label = elt$Label
    )
    bottomRightPoint <- destPoint(st_coordinates(elt)[1, ], 135, sqrt(2) * 100)
    topLeftPoint <- destPoint(st_coordinates(elt)[1, ], 315, sqrt(2) * 100)
    leaf <- addRectangles(
      map = leaf,
      lng1 = topLeftPoint[1],
      lat1 = topLeftPoint[2],
      lng2 = bottomRightPoint[1],
      lat2 = bottomRightPoint[2],
      color = "green"
    )

    color_map <- addRectangles(
      map = color_map,
      lng1 = topLeftPoint[1],
      lat1 = topLeftPoint[2],
      lng2 = bottomRightPoint[1],
      lat2 = bottomRightPoint[2],
      color = "blue"
    )
  }
  observeEvent(input$selectEquimpent, {
    if (!is.null(selected_point())) {
      findSquare(selected_point(), input, plot_data)
    }
  })

  observeEvent(input$selectedEquipementColorMap, {})


  output$table <- renderTable({
    d <- plot_data()
    req(nrow(d) > 0)
    d_ordered <- d[order(d$duree), ]
    code_signification <- setNames(legend$Libelle_TYPEQU, legend$TYPEQU)
    d_ordered$signification <- sapply(d_ordered$typeeq_id, function(x) code_signification[match(x, names(code_signification))])
    d_ordered[, c(15, 5, 7)]
  })

  output$distPlot <- renderPlot({
    f <- plot_data()
    req(nrow(f) > 0)

    h <- hist(f$duree,
      col = "#007bc2", border = "white",
      xlab = "Durée de déplacement d'un carré vers un équipement (en minutes)",
      ylab = "Nombre d'équipements",
      main = "Histogramme de la durée de trajet vers un équipement",
      xaxt = "n",
      yaxt = "n"
    )
    axis(1, at = seq(0, max(f$duree, na.rm = TRUE) + 20, by = 20))
    axis(2, at = seq(0, max(h$counts) + 5, by = 5))
  })

 # output$culturalBudgetToCloseEqPlot <- renderPlot({
  #  createPlotCloseEqToBudget(input, "global_budget")
  #})
  
   #output$culturalBudgetPerInhabitantToCloseEqPlot <- renderPlot({
    # createPlotCloseEqToBudget(input, "budget_per_inhabitant")
  #})

  output$distToCulturalEQPerInhabitantPlot <- renderPlot({
    createBarplotCulturalEq(input)
  })

  print("Server update done!")

  output$map_background <- renderLeaflet({
    leaf
  })
  output$color_map <- renderLeaflet({
    color_map
  })
}


fetchDBColor <- function(input) {
  north_lat <- input$color_map$north
  south_lat <- input$color_map$south
  leafletProxy("color_map") %>%
    clearMarkers() %>%
    clearShapes()

  df <- data.frame(c(input$color_map_bounds$west, input$color_map_bounds$east), c(input$color_map_bounds$north, input$color_map_bounds$south))
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

  eq <- input$selectedEquipementColorMap
  print(eq)
  if (is.null(eq)) {
    eq <- ""
  }
  equipment_type <- dbQuoteString(conn, paste0(eq, "%"))

  incProgress(0.2, detail = "Querying Database")
  res <- dbSendQuery(conn, sprintf("SELECT * FROM equipment_access WHERE \"X\" >= %.0f AND \"X\" <= %.0f AND \"Y\" >= %.0f AND \"Y\" <= %.0f AND \"typeeq_id\" LIKE %s LIMIT %.0f", x_min, x_max, y_min, y_max, equipment_type, max_fetch))

  incProgress(0.4, detail = "Processing Data")
  f <- dbFetch(res)

  if (nrow(f) == 0) {
    print("No squared referenced in this area")
    return()
  }

  f$pop <- f$dist

  data_sf_4326 <- dbCoordsToLeaflet(f)


  incProgress(0.3, detail = "Rendering Map")
  for (elt2 in seq_len(nrow(data_sf_4326))) {
    elt <- data_sf_4326[elt2, ]

    # leafletProxy("color_map") %>% addMarkers(
    # lng = st_coordinates(elt)[,1],
    # lat = st_coordinates(elt)[,2],
    # label = elt$Label
    # )
    color <- "black"
    if (!is.null(elt$Label)) {
      scale <- input$colorMapScale
      if (elt$Label < scale[5]) {
        color <- "purple"
      }
      if (elt$Label < scale[4]) {
        color <- "red"
      }
      if (elt$Label < scale[3]) {
        color <- "orange"
      }
      if (elt$Label < scale[2]) {
        color <- "yellow"
      }
      if (elt$Label < scale[1]) {
        color <- "green"
      }
    }

    bottomRightPoint <- destPoint(st_coordinates(elt)[1, ], 135, sqrt(2) * 100)
    topLeftPoint <- destPoint(st_coordinates(elt)[1, ], 315, sqrt(2) * 100)
    leafletProxy("color_map") %>% addRectangles(
      lng1 = topLeftPoint[1],
      lat1 = topLeftPoint[2],
      lng2 = bottomRightPoint[1],
      lat2 = bottomRightPoint[2],
      color = color,
      label = elt$Label
    )
  }
}
