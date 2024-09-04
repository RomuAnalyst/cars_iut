install.packages('shinyWidgets')
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(readxl)
library(DT)
library(leaflet)
library(httr)
library(shinyWidgets)

# Charger les données depuis le fichier Excel
data <- read_xlsx("CARS.xlsx")
data$Modèle <- trimws(data$Modèle)

ui <- dashboardPage(
  dashboardHeader(title = "FindYourCar", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "vue", icon = icon("info")),
      menuItem("Table des données", tabName = "tab-donnees", icon = icon("database")),
      menuItem("Voiture de ses rêves", tabName = "reve", icon = icon("car")),
      menuItem("Comparateur de voiture", tabName = "comparateur", icon = icon("magnifying-glass")),
      menuItem("Itinéraire en voiture", tabName = "itineraire", icon = icon("road"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      * {color: white !important;}
      .dropdown-menu > li > a:hover,
      .dropdown-menu > li > a:focus {
        background-color: #33386b !important;
        color: white !important;
      }
      .dropdown-menu > li > a {
        background-color: #33386b !important;
        color: white !important;
      }
      .btn.dropdown-toggle {
        background-color: #33386b !important;
        color: white !important;
      }
      .info-box { min-width: 300px; } 
      .leaflet-control-zoom-in, .leaflet-control-zoom-out {
      background-color: #33386b !important; 
      color: white !important; 
      border: none;
      }
    "))
    ),
    shinyDashboardThemes(theme = "purple_gradient"),
    tabItems(
      tabItem("vue",
              imageOutput("myImage")
      ),
      tabItem("tab-donnees",
              h2("Table des données"),
              infoBox("Pays", "Inde", icon = icon("flag"), color = "blue"),
              infoBox("Variables", "19", icon = icon("list-ol"), color = "blue"),
              infoBox("Observations", "203", icon = icon("eye"), color = "blue"),
              dataTableOutput("tableau_complet")
      ),
      tabItem("reve",
              h2("Trouver la voiture de vos rêves"),
              fluidRow(
                column(4, selectInput("marque", label = "Marque choisie", choices = c("Toutes les marques", unique(data$Marque)))),
                column(4, selectInput("carburant", label = "Carburant choisi", choices = c("Toutes les carburants", unique(data$`Type de carburant`)))),
                column(4, selectInput("capacite", label = "Nombre de place", choices = c("Toutes les places", unique(data$`Capacité d'assise`))))
              ),
              fluidRow(
                column(4, selectInput("boite", label = "Type de boite", choices = c("Toutes les boites", unique(data$`Type de transmission`))))
              ),
              infoBox("Modèle", "Modèle de voiture"),
              infoBoxOutput("progressBox"),
              sliderInput("prix", label = "Intervalle de prix (K€)", min = min(data$`Prix moyen`), max = max(data$`Prix moyen`), value = c(min(data$`Prix moyen`), max(data$`Prix moyen`))),
              dataTableOutput("tableau")
      ),
      tabItem("comparateur",
              h2("Comparateur de voitures"),
              fluidRow(
                column(4, pickerInput("voiture1", "Modèle 1", choices = NULL, options = list(`live-search` = TRUE))),
                column(4, pickerInput("voiture2", "Modèle 2", choices = NULL, options = list(`live-search` = TRUE)))
              ),
              tableOutput("table")
              
      ),
      tabItem("itineraire",
              h2("Itinéraire en voiture"),
              sidebarLayout(
                sidebarPanel(
                  textInput("depart", "Adresse de départ :"),
                  textInput("arrivee", "Adresse d'arrivée :"),
                  selectInput("carburant", "Type de carburant :", choices = c("Essence", "Diesel", "Électrique", "CNG")),
                  actionButton("geocoder", "Calculer l'itinéraire"),
                  verbatimTextOutput("debug"),
                  tags$div(
                    style = "margin-top: 20px; font-size: 18px; font-weight: bold;",
                    uiOutput("info_boxes")
                  ),
                  style = "background-color: #33386b; color: white;"
                ),
                mainPanel(
                  leafletOutput("map", width = "100%", height = "600px")
                )
              )
      )
    )
  ),
  title = "Find your car",
  skin = "blue"
)

server <- function(input, output, session) {
  calculer_emission_co2 <- function(type_carburant, distance) {
    emission_par_100km <- c(
      Essence = 21.1,
      Diesel = 17.6,
      Électrique = 5.0,
      CNG = 11.7
    )
    
    if (!type_carburant %in% names(emission_par_100km)) {
      return(NA)
    }
    
    emission_par_km <- emission_par_100km[type_carburant] / 100
    emission_totale <- emission_par_km * distance
    return(emission_totale)
  }
  output$tableau_complet = renderDataTable({
    datatable(data,
              options = list(
                columnDefs = list(list(
                  targets = "_all",
                  className = "dt-center"
                )),
                initComplete = JS(
                  "function() {",
                  "$('#DataTables_Table_0').css({'color': 'white'});",
                  "}")
              )
    )
  })
  
  output$tableau = renderDataTable({
    filtered_data <- data
    
    if (input$marque != "Toutes les marques") {
      filtered_data <- filtered_data[filtered_data$Marque == input$marque, ]
    }
    
    if (input$carburant != "Toutes les carburants") {
      filtered_data <- filtered_data[filtered_data$`Type de carburant` == input$carburant, ]
    }
    if (input$capacite != "Toutes les places") {
      filtered_data <- filtered_data[filtered_data$`Capacité d'assise` == input$capacite, ]
    }
    if (input$boite != "Toutes les boites") {
      filtered_data <- filtered_data[filtered_data$`Type de transmission` == input$boite, ]
    }
    filtered_data <- subset(filtered_data, `Prix moyen` >= input$prix[1] & `Prix moyen` <= input$prix[2])
    filtered_data <- filtered_data[,c(1,2,4,7,12) ]
    datatable(filtered_data,
              options = list(
                columnDefs = list(list(
                  targets = "_all",
                  className = "dt-center"
                )),
                initComplete = JS(
                  "function() {",
                  "$('#DataTables_Table_0').css({'color': 'white'});",
                  "}")
              )
    )
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Marque choisie", paste0(input$marque), icon = icon("list"),
      color = "purple"
    )
  })
  
  output$myImage <- renderImage({
    list(src = "D:/BUT2/SAE Composant decisionnel/action.jpg", width = "100%", height = "120%")
  }, deleteFile = FALSE)
  
  chargerModelesdata <- function() {
    modeles <- unique(data$Modèle)
    updatePickerInput(session, "voiture1", choices = modeles)
    updatePickerInput(session, "voiture2", choices = modeles)
  }
  
  chargerModelesdata()
  
  output$table <- renderTable({
    voiture1 <- data[data$Modèle == input$voiture1, ]
    voiture2 <- data[data$Modèle == input$voiture2, ]
    
    comparison <- rbind(voiture1, voiture2)
    comparison <- t(comparison)
    
    comparison <- comparison[!(rownames(comparison) == "Modèle"), ]
    
    colnames(comparison) <- c(input$voiture1, input$voiture2)
    comparison
  }, rownames = TRUE)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.3522, lat = 48.8566, zoom = 9)
  })
  
  output$debug <- renderPrint({
    req(input$geocoder)
    adresse_depart <- input$depart
    adresse_arrivee <- input$arrivee
    
    debug_text <- ""
    
    url_depart <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(adresse_depart), "&format=json&addressdetails=1")
    response_depart <- GET(url_depart)
    resultats_depart <- content(response_depart, "parsed")
    
    debug_text <- paste("Requête de départ URL:", url_depart, "\nRésultats de départ:", toString(resultats_depart))
    
    if (length(resultats_depart) > 0 && !is.null(resultats_depart[[1]]$lat) && !is.null(resultats_depart[[1]]$lon)) {
      lat_depart <- as.numeric(resultats_depart[[1]]$lat)
      lon_depart <- as.numeric(resultats_depart[[1]]$lon)
      
      url_arrivee <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(adresse_arrivee), "&format=json&addressdetails=1")
      response_arrivee <- GET(url_arrivee)
      resultats_arrivee <- content(response_arrivee, "parsed")
      
      debug_text <- paste(debug_text, "\nRequête d'arrivée URL:", url_arrivee, "\nRésultats d'arrivée:", toString(resultats_arrivee))
      
      if (length(resultats_arrivee) > 0 && !is.null(resultats_arrivee[[1]]$lat) && !is.null(resultats_arrivee[[1]]$lon)) {
        lat_arrivee <- as.numeric(resultats_arrivee[[1]]$lat)
        lon_arrivee <- as.numeric(resultats_arrivee[[1]]$lon)
        
        url_osrm <- paste0("http://router.project-osrm.org/route/v1/driving/", lon_depart, ",", lat_depart, ";", lon_arrivee, ",", lat_arrivee, "?overview=full&geometries=geojson")
        response_osrm <- GET(url_osrm)
        resultats_osrm <- content(response_osrm, "parsed")
        
        debug_text <- paste(debug_text, "\nRequête OSRM URL:", url_osrm, "\nRésultats OSRM:", toString(resultats_osrm))
        
        if (!is.null(resultats_osrm$routes)) {
          coords <- resultats_osrm$routes[[1]]$geometry$coordinates
          if (is.list(coords)) {
            lngs <- sapply(coords, function(x) as.numeric(x[1]))
            lats <- sapply(coords, function(x) as.numeric(x[2]))
            
            leafletProxy("map") %>%
              clearShapes() %>%
              clearMarkers() %>%
              addCircleMarkers(lng = lon_depart, lat = lat_depart, color = "blue", radius = 6, label = "Départ") %>%
              addCircleMarkers(lng = lon_arrivee, lat = lat_arrivee, color = "red", radius = 6, label = "Arrivée") %>%
              addPolylines(lng = lngs, lat = lats, color = "blue") %>%
              fitBounds(lng1 = min(lngs), lat1 = min(lats), lng2 = max(lngs), lat2 = max(lats))
            # Calcul de la distance en kilomètres
            distance_km <- resultats_osrm$routes[[1]]$distance / 1000
            type_carburant <- input$carburant  # Type de carburant sélectionné
            emission_co2 <- calculer_emission_co2(type_carburant, distance_km)  # Calcul des émissions de CO2
            
            # Calcul de la durée en heures et minutes
            duration <- resultats_osrm$routes[[1]]$duration
            hours <- floor(duration / 3600)
            minutes <- round((duration %% 3600) / 60)
            
            duration_text <- if (hours > 0) {
              paste(hours, "heures", minutes, "minutes")
            } else {
              paste(minutes, "minutes")
            }
            
            output$info_boxes <- renderUI({
              fluidRow(
                column(width = 12,
                       infoBox(
                         title = "Durée",
                         value = duration_text,
                         icon = icon("clock"),
                         color = "blue"
                       )
                ),
                column(width = 12,
                       infoBox(
                         title = "Distance",
                         value = paste(round(resultats_osrm$routes[[1]]$distance / 1000, 1), "kilomètres"),
                         icon = icon("road"),
                         color = "purple"
                       )
                ),
                column(width = 12,  # Nouvelle infoBox pour les émissions de CO2
                       infoBox(
                         title = "Émissions de CO2",
                         value = paste(round(emission_co2, 2), "kg de CO2"),
                         icon = icon("leaf"),
                         color = "green"
                       )
                )
              )
            })
          } else {
            debug_text <- paste(debug_text, "\nErreur: Les coordonnées de l'itinéraire ne sont pas dans le format attendu.")
          }
        } else {
          debug_text <- paste(debug_text, "\nErreur OSRM:", resultats_osrm$message)
        }
      } else {
        debug_text <- paste(debug_text, "\nErreur: Aucune adresse d'arrivée trouvée ou mal formatée.")
      }
    } else {
      debug_text <- paste(debug_text, "\nErreur: Aucune adresse de départ trouvée ou mal formatée.")
    }
    
    return(debug_text)
  })
}

shinyApp(ui = ui, server = server)