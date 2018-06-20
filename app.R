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
library(DT)

## SOURCES ####
source('global.R')
source('modules/mod_dataUI.R')
source('modules/mod_mapUI.R')
source('modules/mod_vizInput.R')
source('modules/mod_infoPanelOutput.R')

# source('modules/mod_baseMapOutput.R')
# source('modules/mod_shapeCondPanelUI.R')
# source('modules/mod_mapControlsInput.R')
# source('modules/mod_dataReactiveOutput.R')
# source('modules/mod_filterAndSelUI.R')
# source('modules/mod_aggregationInput.R')
# source('modules/mod_baseTableOutput.R')

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
        
        # mod_data module, it includes the dataSel, dataFil and dataAgg inputs
        mod_dataUI('mod_dataUI'),

        # mod_map, it includes the map
        mod_mapUI('mod_mapUI'),
        
        # mod_infoPanel, it includes the map events info panel
        mod_infoPanelOutput('mod_infoPanelOutput'),
        
        # cite div
        tags$div(
          id = 'cite',
          "Dades compilats pel CREAF & CTFC basats en l'IFN"
        )
      )
    )
    
    # data tab
    # tabPanel(
    #   "Explora les dades",
    #   
    #   # row for inputs, already in the module
    #   # mod_aggregationInput('aggregation'),
    #   
    #   # rows for tables
    #   mod_baseTableOutput('tables_outputs')
    # )
  )
)

## SERVER ####
server <- function(input, output, session) {
  
  # module calling
  # data
  data_reactives <- callModule(
    mod_data, 'mod_dataUI'
  )
  
  # viz controls
  viz_reactives <- callModule(
    mod_viz, 'mod_vizInput',
    data_reactives
  )
  
  # map
  map_reactives <- callModule(
    mod_map, 'mod_mapUI',
    data_reactives, viz_reactives
  )
  
  # info panel
  callModule(
    mod_infoPanel, 'mod_infoPanelOutput',
    data_reactives, map_reactives
  )
  
  # #### interactive map ####
  # # controls module, see mod_mapControlsInput.R file for more info
  # map_controls <- callModule(
  #   mod_mapControls, 'map_controls'
  # )
  # 
  # # filter and select module
  # fill_and_sel <- callModule(
  #   mod_filterAndSel, 'fil_and_sel',
  #   control_inputs = map_controls,
  #   noms = list(
  #     comarca = c('Totes', sort(as.character(polygons_comarques@data$NOM_COMAR))),
  #     municipi = c('Tots', sort(as.character(polygons_municipis@data$NOM_MUNI))),
  #     vegueria = c('Totes', sort(as.character(polygons_vegueries@data$NOMVEGUE))),
  #     provincia = c('Totes', sort(as.character(polygons_provincies@data$NOM_PROV)))
  #   )
  # )
  # 
  # # data module
  # data_parcelas <- callModule(
  #   mod_dataReactive, 'data_parcelas', map_controls = map_controls,
  #   filterAndSel_inputs = fill_and_sel
  #   
  # )
  # 
  # # see mod_baseMapOutput.R file for more info about map widget
  # ifn_map <- callModule(
  #   mod_baseMap, 'ifn_map',
  #   municipis = polygons_municipis, comarques = polygons_comarques,
  #   vegueries = polygons_vegueries, provincies = polygons_provincies,
  #   map_controls = map_controls, data = data_parcelas
  # )
  # 
  # # conditional panel for shapes info (parcelas y territorios)
  # callModule(
  #   mod_shapeCondPanel, 'info_panel',
  #   map_inputs = ifn_map, data = data_parcelas
  # )
  # 
  # # module for aggregation inputs
  # aggregation_controls <- callModule(
  #   mod_aggregation, 'aggreg',
  #   mapControls = map_controls
  # )
  # 
  # # module for the table
  # tableBase <- callModule(
  #   mod_baseTable, 'tables_outputs',
  #   mapControls = map_controls, aggregation = aggregation_controls,
  #   dataReactive = data_parcelas, filterAndSel = fill_and_sel
  # )
  # 
  # # observers to toggle the ancillary panels
  # observeEvent(
  #   eventExpr = map_controls$show_sel,
  #   handlerExpr = {
  #     toggle('filterAndSel')
  #   }
  # )
  # 
  # observeEvent(
  #   eventExpr = map_controls$show_agg,
  #   handlerExpr = {
  #     toggle('aggregation')
  #   }
  # )
  # 
  # observeEvent(
  #   eventExpr = map_controls$show_inf,
  #   handlerExpr = {
  #     toggle('infoPanel')
  #   }
  #   
  # )
  # 
  # # observer to toggle the "conditional shape panel"
  # observeEvent(
  #   eventExpr = ifn_map$shape_click,
  #   handlerExpr = {
  #     shinyjs::show(
  #       id = "infoPanel", anim = TRUE, animType = 'fade', time = 0.5
  #     )
  #   }
  # )
  # 
  # 
  # ### debug #####
  # # output$debug1 <- renderPrint({
  # #   aggregation_controls$aggregation_level
  # # })
  # # output$debug2 <- renderPrint({
  # #  tableBase$table_name()
  # # })
  # # output$debug3 <- renderPrint({
  # #   map_controls$shape_click
  # # })
}

# Run the application 
shinyApp(ui = ui, server = server)

