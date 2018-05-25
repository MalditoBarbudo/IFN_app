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
  'Precipitació' = 'precipitacioanual',
  'Temperatura' = 'temperaturamitjanaanual'
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
        
        selectInput('color', 'Color', vars),
        selectInput('size', 'Mida', vars)
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
        opacity = 1.0, fill = FALSE,
        label = ~NOMMUNI,
        # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = TRUE,
                                            fill = FALSE)
      ) %>%
      addPolygons(
        data = polygons_comarques, group = 'Comarques',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = FALSE,
        label = ~NOMCOMAR,
        # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = TRUE,
                                            fill = FALSE)
      ) %>%
      addPolygons(
        data = polygons_vegueries, group = 'Vegueries',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = FALSE,
        label = ~NOMVEGUE,
        # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = TRUE,
                                            fill = FALSE)
      ) %>%
      addPolygons(
        data = polygons_provincies, group = 'Provincies',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = FALSE,
        label = ~NOMPROV,
        # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = TRUE,
                                            fill = FALSE)
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
      select(idparcela, precipitacioanual, temperaturamitjanaanual) %>%
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
    
    # color palette
    color_vector <- data_parcelas() %>%
      pull(color_var)
    pal <- colorBin('viridis', color_vector, 9)
    
    # size palette
    # size_vector <- data_parcelas[[size_var]] / max(data_parcelas[[size_var]]) * 3000
    
    
    # update map
    leafletProxy('ifn_map', data = data_parcelas()) %>%
      addCircles(
        group = 'Parcelas', lng = ~ long, lat = ~ lat,
        layerId = ~idparcela, stroke = FALSE, fillOpacity = 0.4,
        fillColor = pal(color_vector), radius = 500
      ) %>%
      addLegend(
        position = 'bottomright', pal = pal, values = color_vector,
        title = color_var, layerId = 'color_legend'
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

