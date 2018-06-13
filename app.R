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
source('modules/mod_dataReactiveOutput.R')
source('modules/mod_filterAndSelUI.R')
source('modules/mod_tableControlsInput.R')

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
        
        # input controls
        absolutePanel(
          id = 'controls', class = 'panel panel-default', fixed = TRUE,
          draggable = TRUE, width = 330, height = 'auto',
          # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
          # top = 'auto', left = 'auto', right = 100, bottom = 100,
          top = 250, left = 'auto', right = 15, bottom = 'auto',
          
          # panel title
          h2('Juega con el mapa'),
          
          # modules to include
          # controls
          mod_mapControlsInput('map_controls')
          
        ),
        
        # map module
        mod_baseMapOutput('ifn_map'),
        
        # info panel
        # conditional panel for shape info
        mod_shapeCondPanelUI('info_shape'),
        
        # filter and select module
        hidden(
          absolutePanel(
            id = 'filandsel', class = 'panel, panel-default', fixed = TRUE,
            draggable = TRUE, width = 550, height = 'auto',
            # top = 'auto', left = 'auto', right = 100, bottom = 100,
            top = 100, left = 100, rigth = 'auto', bottom = 'auto',
            
            h2('Selecciona les parcel·les a analitzar'),
            
            # module
            mod_filterAndSelUI('fil_and_sel')
            
          )
        ),
        
        tags$div(
          id = 'cite',
          "Dades compilats pel CREAF & CTFC basats en l'IFN"
        )
      )
    ),
    
    # data tab
    tabPanel(
      "Explora les dades",
      
      # row for inputs, already in the module
      mod_tableControlsInput('table_controls')
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
  
  # filter and select module
  fill_and_sel <- callModule(
    mod_filterAndSel, 'fil_and_sel',
    control_inputs = map_controls,
    noms = list(
      comarca = c('Totes', sort(as.character(polygons_comarques@data$NOM_COMAR))),
      municipi = c('Tots', sort(as.character(polygons_municipis@data$NOM_MUNI))),
      vegueria = c('Totes', sort(as.character(polygons_vegueries@data$NOMVEGUE))),
      provincia = c('Totes', sort(as.character(polygons_provincies@data$NOM_PROV)))
    )
  )
  
  # data module
  data_parcelas <- callModule(
    mod_dataReactive, 'data_parcelas', map_controls = map_controls,
    filterAndSel_inputs = fill_and_sel
    
  )
  
  # see mod_baseMapOutput.R file for more info about map widget
  ifn_map <- callModule(
    mod_baseMap, 'ifn_map',
    municipis = polygons_municipis, comarques = polygons_comarques,
    vegueries = polygons_vegueries, provincies = polygons_provincies,
    map_controls = map_controls, data = data_parcelas
  )
  
  # conditional panel for shapes info (parcelas y territorios)
  callModule(
    mod_shapeCondPanel, 'info_shape',
    map_inputs = ifn_map, data = data_parcelas
  )
  
  # module for tables inputs
  tableControls <- callModule(
    mod_tableControls, 'table_controls',
    mapControls = map_controls
  )
  
  # observer to toggle the select module panel
  observeEvent(
    eventExpr = map_controls$show_sel,
    handlerExpr = {
      toggle('filandsel')
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

