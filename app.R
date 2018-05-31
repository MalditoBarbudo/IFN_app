library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(viridis)

## SOURCES ####
source('global.R', local = TRUE)

## VARS ####
vars <- c(
  'Cap' = '',
  'Precipitació Anual' = 'precipitacioanual',
  'Temperatura Minima Anual' = 'temperaturaminimaanual',
  'Temperatura Mitjana Anual' = 'temperaturamitjanaanual',
  'Temperatura Maxima Anual' = 'temperaturamaximaanual',
  'Radiació Anual' = 'radiacioanual',
  'Altitud' = 'altitud',
  'Orientació' = 'orientacio',
  'Pendent (%)' = 'pendentpercentatge'
)

ifns <- c(
  'IFN 2' = 'ifn2',
  'IFN 3' = 'ifn3'#,
  # 'IFN 4' = 'ifn4',
  
)

## UI ####
ui <- navbarPage(
  # opts
  title = "Eines d'anàlisi IFN",
  id = 'nav',
  collapsible = TRUE,
  
  # contents
  tabPanel(
    "Mapa interactiu",
    
    div(
      class = "outer",
      tags$head(
        # custom css
        includeCSS('resources/ifn.css')
        # custom scripts
        # includeScript("resources/gomap.js")
      ),
      
      # map output
      leafletOutput(
        'ifn_map', width = '100%', height = '100%'
      ),
      
      # overlay panel with controls for color & size
      absolutePanel(
        id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 320, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',
        
        h2("Pinta y Colorea"),
        
        selectInput(
          'color', 'Color', vars, selected = 'Temperatura Mitjana Anual'
        ),
        selectInput('size', 'Mida', vars, selected = 'Cap'),
        checkboxInput('inverse_pal', 'Invert palette', value = FALSE)
      ),
      
      # overlay panel with controls for ifn data
      absolutePanel(
        id = 'ifnsel', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 320, left = 20, right = 'auto', bottom = 'auto',
        width = 330, height = 'auto',
        
        h2("Dades IFN"),
        
        selectInput('ifn', 'Versiò', ifns)
      ),
      
      tags$div(
        id = 'cite',
        "Dades compilats pel CREAF & CTFC basats en l'IFN"
      )
    )
  )
)

## SERVER ####
server <- function(input, output, session) {
  
  # interactive map ####
  output$ifn_map <- renderLeaflet({
    
    leaflet() %>%
      # addProviderTiles(providers$Hydda.Base, group = 'Base') %>%
      setView(1.519410, 41.720509, zoom = 8) %>%
      addPolygons(
        data = polygons_municipis, group = 'Municipis',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMMUNI,
        fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = polygons_comarques, group = 'Comarques',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMCOMAR,
        fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = polygons_vegueries, group = 'Vegueries',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMVEGUE,
        fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = polygons_provincies, group = 'Provincies',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMPROV,
        fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addLayersControl(
        baseGroups = c('Provincies', 'Vegueries',
                       'Comarques', 'Municipis'),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  # reactive for generate data for the different IFNs
  data_parcelas <- reactive({
    
    clima_name <- paste0('parcela', input$ifn, '_clima')
    sig_name <- paste0('parcela', input$ifn, '_sig')
    
    data_parcelas <- tbl(oracle_ifn, clima_name) %>%
      # select(idparcela, precipitacioanual, temperaturamitjanaanual) %>%
      inner_join(tbl(oracle_ifn, sig_name), by = 'idparcela') %>%
      collect()
    
    coordinates_parcelas <- data_parcelas[,c('idparcela', 'utm_x', 'utm_y')]
    coordinates(coordinates_parcelas) <- ~utm_x+utm_y
    proj4string(coordinates_parcelas) <- CRS("+init=epsg:25831")
    
    coordinates_par_transf <- spTransform(
      coordinates_parcelas, CRS("+proj=longlat +datum=WGS84")
    )
    
    data_parcelas %>%
      mutate(
        long = coordinates_par_transf@coords[,1],
        lat = coordinates_par_transf@coords[,2]
      )
    
  })
  
  # observer to maintain the color of polygons
  observe({
    color_var <- input$color
    size_var <- input$size
    data_par <- data_parcelas()
    
    if (color_var == '') {
      color_vector <- rep('parcela', nrow(data_par))
      pal <- colorFactor('viridis', color_vector)
    } else {
      # color palette
      color_vector <- data_par %>%
        pull(color_var)
      pal <- colorBin('viridis', color_vector, 9, reverse = input$inverse_pal)
    }
    
    if (size_var ==  '') {
      size_vector <- rep(1000, nrow(data_par))
    } else {
      # size palette
      size_vector <- data_par[[size_var]] / max(data_par[[size_var]]) * 3000
    }
    
    # update map
    leafletProxy('ifn_map', data = data_par) %>%
      addCircles(
        group = 'Parcelas', lng = ~ long, lat = ~ lat,
        layerId = ~idparcela, stroke = FALSE, fillOpacity = 0.4,
        fillColor = pal(color_vector), radius = size_vector
      ) %>%
      addLegend(
        position = 'bottomright', pal = pal, values = color_vector,
        title = color_var, layerId = 'color_legend'
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

