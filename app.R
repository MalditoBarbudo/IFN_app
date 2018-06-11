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
          draggable = TRUE, top = 100, left = 100, rigth = 'auto', bottom = 'auto',
          width = 330, height = 'auto',
          
          # panel title
          h2('Juega con el mapa'),
          
          # modules to include
          # controls
          mod_mapControlsInput('map_controls'),
          
          # conditional panel for shape info
          mod_shapeCondPanelUI('info_shape')
        ),
        
        # map module
        mod_baseMapOutput('ifn_map'),
        
        # filter and select module
        absolutePanel(
          id = 'filandsel', class = 'panel, panel-default', fixed = TRUE,
          draggable = TRUE, top = 'auto', left = 'auto', right = 100, bottom = 100,
          width = 650, height = 'auto',
          
          h2('Filtra i selecciona'),
          
          # module
          mod_filterAndSelUI('fil_and_sel')
          
        ),
        
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
  
  # data module
  data_parcelas <- callModule(
    mod_dataReactive, 'data_parcelas', ifn = reactive(map_controls$ifn)
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
  
  # filter and select module
  callModule(
    mod_filterAndSel, 'fil_and_sel',
    control_inputs = map_controls,
    noms = list(
      comarques = c('Totes', sort(as.character(polygons_comarques@data$NOM_COMAR))),
      municipis = c('Tots', sort(as.character(polygons_municipis@data$NOM_MUNI))),
      vegueries = c('Totes', sort(as.character(polygons_vegueries@data$NOMVEGUE))),
      provincies = c('Totes', sort(as.character(polygons_provincies@data$NOM_PROV)))
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

