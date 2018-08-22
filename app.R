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
library(patchwork)
library(shinycssloaders)
library(glue)
library(tidyIFN)
library(IFNappkg)

## SOURCES ####
# source('global.R')

## app ####
tidy_app()

# ## UI ####
# ui <- tagList(
#   
#   useShinyjs(),
#   shinyWidgets::chooseSliderSkin(skin = "Flat", color = '#0DB3D4'),
#   
#   navbarPage(
#     # opts
#     title = "Eines d'anàlisi IFN",
#     id = 'nav',
#     collapsible = TRUE,
#     
#     # contents
#     tabPanel(
#       "Mapa interactiu",
#       
#       div(
#         class = "outer",
#         tags$head(
#           # custom css
#           includeCSS('resources/ifn.css')
#           # custom scripts
#           # includeScript("resources/gomap.js")
#         ),
#         
#         ########################################################### debug ######
#         # absolutePanel(
#         #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
#         #   draggable = TRUE, width = 640, height = 'auto',
#         #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
#         #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
#         #   top = 60, left = 'auto', right = 50, bottom = 'auto',
#         # 
#         #   textOutput('debug1'),
#         #   textOutput('debug2'),
#         #   textOutput('debug3')
#         # ),
#         ########################################################### end debug ##
#         
#         ## mod_data ####
#         # mod_data module, it includes the dataSel, dataFil and dataAgg inputs
#         mod_dataInput('mod_dataInput'),
# 
#         ## mod_map ####
#         # mod_map, it includes the map
#         mod_mapUI('mod_mapUI'),
#         
#         ## mod_infoPanel ####
#         # mod_infoPanel, it includes the map events info panel
#         mod_infopanelUI('mod_infopanelUI'),
#         
#         ## mod_advancedFilters ####
#         mod_advancedFiltersUI('mod_advancedFiltersUI'),
#         
#         ## cite div ####
#         tags$div(
#           id = 'cite',
#           "Dades compilats pel CREAF & CTFC basats en l'IFN"
#         )
#       )
#     ),
#     
#     # data tab
#     tabPanel(
#       "Explora les dades",
# 
#      mod_tableOutput('mod_tableOutput')
#     ),
#     
#     # Alometrias tab
#     tabPanel(
#       "Alometrías"
#     )
#   )
# )
# 
# ## SERVER ####
# server <- function(input, output, session) {
#   
#   ## module calling ####
#   # data
#   data_reactives <- callModule(
#     mod_data, 'mod_dataInput'
#   )
#   
#   # advancedFilters
#   advancedFIlters_reactives <- callModule(
#     mod_advancedFilters, 'mod_advancedFiltersUI',
#     data_reactives
#   )
#   
#   # map
#   map_reactives <- callModule(
#     mod_map, 'mod_mapUI',
#     data_reactives, advancedFIlters_reactives, ifndb
#   )
#   
#   # info panel
#   callModule(
#     mod_infopanel, 'mod_infopanelUI',
#     data_reactives, map_reactives, advancedFIlters_reactives, ifndb
#   )
#   
#   callModule(
#     mod_table, 'mod_tableOutput',
#     data_reactives, advancedFIlters_reactives
#   )
#   
#   ## debug #####
#   # output$debug1 <- renderPrint({
#   #   data_reactives$show_adv_fils
#   # })
#   # output$debug2 <- renderPrint({
#   #   get_scenario(data_reactives$viz_shape, data_reactives$agg_level)
#   # })
#   # output$debug3 <- renderPrint({
#   #   data_reactives$espai_tipus_fil
#   # })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
