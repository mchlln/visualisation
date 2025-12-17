library(shiny)

ui <- page_sidebar(
    title = "Visualisation 2025",
    sidebar = sidebar(
        position = "left",
        sliderInput("slider", label = "Max number of areas to show", min = 1000, max = 50000, value = 10000),
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
                            label="Select Equipment category",
                            choices = list("GLOBAL" = ".",
                                           "SERVICES POUR LES PARTICULIERS " = "A",
                                           "COMMERCES" = "B",
                                           "ENSEIGNEMENT" = "C",
                                           "SANTÉ ET ACTION SOCIALE" = "D",
                                           "TRANSPORTS ET DÉPLACEMENTS " ="E",
                                           "SPORTS, LOISIRS ET CULTURE "="F",
                                           "TOURISME"="G"),
                            selected = "."
                ),
                plotOutput(outputId = "distPlot"),
                div(style = "overflow-y: auto; height: 300px;", tableOutput(outputId = "table"))
            )
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


