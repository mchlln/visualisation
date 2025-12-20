library(shiny)
library(shinyWidgets)

ui <- page_sidebar(
  tags$head(
    tags$style(HTML("
      #colorMapScale .noUi-connects .noUi-connect:nth-child(1) { background: green; }
      #colorMapScale .noUi-connects .noUi-connect:nth-child(2) { background: yellow; }
      #colorMapScale .noUi-connects .noUi-connect:nth-child(3) { background: orange; }
      #colorMapScale .noUi-connects .noUi-connect:nth-child(4) { background: red; }
      #colorMapScale .noUi-connects .noUi-connect:nth-child(5) { background: purple; }
      #colorMapScale .noUi-connects .noUi-connect:nth-child(6) { background: black; }
    "))
  ),
  title = "Visualisation 2025",
  sidebar = sidebar(
    position = "left",
    sliderInput("slider", label = "Max number of areas to show", min = 1000, max = 500000, value = 10000),
  ),
  navset_card_underline(
    nav_panel(
      title = "General view",
      layout_columns(
        card(
          card_header("Map of Equipment Access"),
          leafletOutput("map_background"),
        ),
        card(
          card_header("Travel Time Distribution"),
          selectInput("selectEquimpent",
            label = "Select Equipment category",
            choices = list(
              "GLOBAL" = ".",
              "SERVICES POUR LES PARTICULIERS " = "A",
              "COMMERCES" = "B",
              "ENSEIGNEMENT" = "C",
              "SANTÉ ET ACTION SOCIALE" = "D",
              "TRANSPORTS ET DÉPLACEMENTS " = "E",
              "SPORTS, LOISIRS ET CULTURE " = "F",
              "TOURISME" = "G"
            ),
            selected = "."
          ),
          plotOutput(outputId = "distPlot"),
          div(style = "overflow-y: auto; height: 300px;", tableOutput(outputId = "table"))
        )
      )
    ),
    nav_panel(
      title = "Heatmap",
      layout_columns(
        leafletOutput("color_map"),
        card(
          selectizeInput(
            inputId = "selectedEquipementColorMap",
            label = "Select an equipement",
            choices = setNames(legend$TYPEQU, legend$Libelle_TYPEQU),
            selected = NULL,
            multiple = FALSE,
            options = NULL
          ),
          switchInput(inputId = "i2", value = TRUE, "Use default color pallet", width = "100%", inline = T, size = "large"),
          noUiSliderInput(
            inputId = "colorMapScale",
            label = "Select:",
            min = 0,
            max = 150,
            value = c(10, 20, 50, 100, 125),
            tooltips = TRUE,
            step = 1,
            width = "100%",
            color = "#007bc2",
            update_on = "end",
            connect = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
            format = wNumbFormat(decimals = 0)
          ),
          layout_columns(
            col_widths = c(1, 1),
            actionButton(
              inputId = "cancel",
              label = "reset",
              width = "30%"
            ),
            actionButton(
              inputId = "test",
              label = "Update Color Pallet",
              width = "75%"
            )
          )
        ),
      )
    ),
    nav_panel(
      title = "About",
      card(
        card_header("Information"),
        p("This General view tab displays equipment access and travel time distributions across various regions."),
        p("Other tabs containing different visualisations will be added later"),
        p("On the sidebar, you can choose to display more squares")
      )
    )
  )
)
