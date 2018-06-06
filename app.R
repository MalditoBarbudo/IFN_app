library(shiny)
library(shinyjs)
library(leaflet)
library(sp)
library(rgdal)
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(pool)
library(viridis)

## SOURCES ####
source('global.R')
source('modules/mod_baseMapOutput.R')
source('modules/mod_shapeCondPanelUI.R')
source('modules/mod_mapControlsInput.R')

## VARS ####
# vars <- c(
#   'Cap' = '',
#   'Precipitació Anual' = 'precipitacioanual',
#   'Temperatura Minima Anual' = 'temperaturaminimaanual',
#   'Temperatura Mitjana Anual' = 'temperaturamitjanaanual',
#   'Temperatura Maxima Anual' = 'temperaturamaximaanual',
#   'Radiació Anual' = 'radiacioanual',
#   'Altitud' = 'altitud',
#   'Orientació' = 'orientacio',
#   'Pendent (%)' = 'pendentpercentatge'
# )

# ifns <- c(
#   'IFN 2' = 'ifn2',
#   'IFN 3' = 'ifn3'#,
#   # 'IFN 4' = 'ifn4',
#   
# )

## UI ####
ui <- tagList(
  
  useShinyjs(),
  
  navbarPage(
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
        
        # # overlay panel with controls for color & size
        # absolutePanel(
        #   id = 'controls', class = 'panel panel-default', fixed = TRUE,
        #   draggable = TRUE, top = 320, left = 'auto', right = 20, bottom = 'auto',
        #   width = 330, height = 'auto',
        #   
        #   h2("Pinta y Colorea"),
        #   
        #   selectInput(
        #     'color', 'Color', vars, selected = 'temperaturamitjanaanual'
        #   ),
        #   selectInput('size', 'Mida', vars, selected = 'Cap'),
        #   checkboxInput('inverse_pal', 'Invert palette', value = FALSE)
        # ),
        # 
        # # overlay panel with controls for ifn data
        # absolutePanel(
        #   id = 'ifnsel', class = 'panel panel-default', fixed = TRUE,
        #   draggable = TRUE, top = 320, left = 20, right = 'auto', bottom = 'auto',
        #   width = 330, height = 'auto',
        #   
        #   h2("Dades IFN"),
        #   
        #   selectInput('ifn', 'Versiò', ifns)
        # ),
        
        # controls
        mod_mapControlsInput('map_controls'),
        
        # map output
        mod_baseMapOutput('ifn_map'),
        
        # conditional panel for shape info
        # mod_shapeCondPanelUI('info_shape'),
        
        tags$div(
          id = 'cite',
          "Dades compilats pel CREAF & CTFC basats en l'IFN"
        )
      )
    )
  )
)

## SERVER ####
server <- function(input, output, session) {
  
  #### interactive map ####
  # controls module, see mod_mapControlsInput.R file for more info
  map_controls <- callModule(
    mod_mapControls, 'map_controls'
  )
  
  # see mod_baseMapOutput.R file for more info about map widget
  ifn_map <- callModule(
    mod_baseMap, 'ifn_map',
    municipis = polygons_municipis, comarques = polygons_comarques,
    vegueries = polygons_vegueries, provincies = polygons_provincies,
    map_controls = map_controls
  )
  
  # conditional panel for shapes info (parcelas y territorios)
  # callModule(
  #   mod_shapeCondPanel, 'info_shape', data = map_controls$data_parcelas,
  #   baseMap_reactives = ifn_map
  # )
}

# Run the application 
shinyApp(ui = ui, server = server)

