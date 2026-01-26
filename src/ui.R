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
    ")),
    tags$script(HTML("
      var originalGetContext = HTMLCanvasElement.prototype.getContext;
      HTMLCanvasElement.prototype.getContext = function(type, attributes) {
        if (type === 'webgl' || type === 'experimental-webgl' || type === 'webgl2') {
          attributes = attributes || {};
          attributes.preserveDrawingBuffer = true;
        }
        return originalGetContext.call(this, type, attributes);
      };
    ")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
      $(document).on('click', '#save_btn', function() {
        var mapLoc = document.getElementById('color_map');
        html2canvas(mapLoc, {
          useCORS: true,
          allowTaint: true,
          ignoreElements: function(element) {
            return element.classList.contains('leaflet-control-zoom');
          }
        }).then(function(canvas) {
          var link = document.createElement('a');
          document.body.appendChild(link);
          link.download = 'color_map.png';
          link.href = canvas.toDataURL('image/png');
          link.click();
          document.body.removeChild(link);
        });
      });
      $(document).on('click', '#open_dist_plot', function() {
        var img = $('#distPlot img');
        if (img.length > 0) {
          var src = img.attr('src');
          var w = window.open(\"\");
          w.document.write('<img src=\"' + src + '\" style=\"width:100%\"/>');
          w.document.close();
        }
      });
    "))
  ),
  title = "Visualisation 2025",
  sidebar = sidebar(
    position = "left",
    sliderInput("slider", label = "Nombre maximum de carrés à afficher", min = 1000, max = 500000, value = 10000),
    textOutput("text"),
    p("Rafraichissement automatique de la carte"),
    #switchInput(label = "Rafraichissement automatique de la carte", inputId = "auto_refresh1", value = TRUE),
    switchInput( inputId = "auto_refresh", value = TRUE)
  ),
  navset_card_underline(
    id = "main_nav",
    nav_panel(
      title = "Vue Générale",
      layout_columns(
        card(
          card_header("Carte d'accès aux équipements"),
          leafletOutput("map_background"),
        ),
        card(
          card_header("Distribution du temps de trajet"),
          selectInput("selectEquimpent",
            label = "Sélectionner une Catégorie",
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
          navset_card_underline(
            nav_panel(
              title = "Graphique",
              actionButton("open_dist_plot", "Agrandir", icon = icon("expand"), class = "btn-sm", style = "margin-bottom: 10px;"),
              plotOutput(outputId = "distPlot"),
            ),
            nav_panel(
              title = "Tableau",
              div(style = "overflow-y: auto; height: 100%;", tableOutput(outputId = "table"))
            )
          ),
          
          #
        )
      )
    ),
    nav_panel(
      title = "Distance aux équipements",
      layout_columns(
        leafletOutput("color_map"),
        card(
          selectizeInput(
            inputId = "selectedEquipementColorMap",
            label = "Sélectionner un équipement",
            choices = setNames(legend$TYPEQU, legend$Libelle_TYPEQU),
            selected = NULL,
            multiple = FALSE,
            options = NULL
          ),
          noUiSliderInput(
            inputId = "colorMapScale",
            label = "Sélectionner:",
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
              label = "Réinitialiser",
              width = "100%"
            ),
            actionButton(
              inputId = "test",
              label = "Mettre à jour les couleurs",
              width = "100%"
            )
          ),
          actionButton(
            inputId = "save_btn",
            label = "Save Map as PNG",
            icon = icon("download"),
            width = "100%",
            style = "margin-top: 10px; background-color: #007bc2; color: white;"
          )
        ),
      )
    ),
    nav_panel(
      title = "Accès à la culture",
      # we don't see a correlation, not shown in the app but in the presentation
      # card(
      # plotOutput(outputId= "culturalBudgetToCloseEqPlot")
      # ),
      # card(
      # plotOutput(outputId= "culturalBudgetPerInhabitantToCloseEqPlot")
      # ),
      card(
        # select box to choose an equipment to display
        plotOutput(outputId = "distToCulturalEQPerInhabitantPlot")
      )
    ),
    nav_panel(
      title = "À propos",
      card(
        card_header("Information"),
        p("L'onglet vue générale permet de visualiser l'accès aux équipements en cliquant sur un carré de la carte"),
        p("La Carte de chaleur permet de voir la distance à un équipement donné sur chaque carré. Cela permet ed voir la répartition des équipements sur le territoire."),
        p("L'onglet d'accès à la culture permet de visualiser la distance aux différents équipements culturels selon la taille des villes. On se base sur toutes les données chargées dans la base de données."),
        p("La barre latérale permet de choisir le ,nombre de carrés à afficher, plus le nombre est important, plus le temps de chargement est élevé. ")
      )
    )
  )
)
