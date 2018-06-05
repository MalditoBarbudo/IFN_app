library(shiny)
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
      
      # overlay panel with controls for color & size
      absolutePanel(
        id = 'controls', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 320, left = 'auto', right = 20, bottom = 'auto',
        width = 330, height = 'auto',
        
        h2("Pinta y Colorea"),
        
        selectInput(
          'color', 'Color', vars, selected = 'temperaturamitjanaanual'
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
      
      # conditional panel for shape info
      mod_shapeCondPanelUI('info_shape'),
      
      # map output
      mod_baseMapOutput('ifn_map'),
      
      tags$div(
        id = 'cite',
        "Dades compilats pel CREAF & CTFC basats en l'IFN"
      )
    )
  )
)

## SERVER ####
server <- function(input, output, session) {
  
  #### interactive map ####
  # data
  data_parcelas <- reactive({
    
    ifn_sel <- input$ifn
    
    # tables names, depending on the ifn selected
    clima_name <- paste0('parcela', ifn_sel, '_clima')
    sig_name <- paste0('parcela', ifn_sel, '_sig_etrs89')
    cec_name <- paste0('r_cadesclcon_', ifn_sel)
    
    # table for
    #   1. parcelas
    #   2. colores y tamaños
    #   3. popups
    tbl(oracle_ifn, clima_name) %>%
      inner_join(tbl(oracle_ifn, sig_name), by = 'idparcela') %>%
      inner_join(tbl(oracle_ifn, cec_name), by = 'idparcela') %>%
      # select(idparcela) %>%
      collect()
  })
  
  # see mod_baseMapOutput.R file for more info about map widget
  callModule(
    mod_baseMap, 'ifn_map',
    municipis = polygons_municipis, comarques = polygons_comarques,
    vegueries = polygons_vegueries, provincies = polygons_provincies,
    size = reactive(input$size), color = reactive(input$color),
    inv_pal = reactive(input$inverse_pal), data_parcelas = data_parcelas
  )
  
  # conditional panel for shapes info (parcelas y territorios)
  callModule(
    mod_shapeCondPanel, 'info_shape', data = data_parcelas
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

