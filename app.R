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
        
        ########################################################### debug ######
        absolutePanel(
          id = 'debug', class = 'panel panel-default', fixed = TRUE,
          draggable = TRUE, width = 640, height = 'auto',
          # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
          # top = 'auto', left = 'auto', right = 100, bottom = 100,
          top = 60, left = 'auto', right = 50, bottom = 'auto',

          textOutput('debug1'),
          textOutput('debug2'),
          textOutput('debug3')
        ),
        ########################################################### end debug ##
        
        ## vizControls ####
        absolutePanel(
          id = 'vizControls', class = 'panel panel-default', fixed = TRUE,
          draggable = TRUE, width = 320, height = 'auto',
          top = 60, right = 'auto', left = 700, bottom = 'auto',
          
          mod_vizInput('mod_vizInput')
        ),
        
        ## mod_data ####
        # mod_data module, it includes the dataSel, dataFil and dataAgg inputs
        mod_dataUI('mod_dataUI'),

        ## mod_map ####
        # mod_map, it includes the map
        mod_mapUI('mod_mapUI'),
        
        ## mod_infoPanel ####
        # mod_infoPanel, it includes the map events info panel
        disabled(
          hidden(
            div(
              id = 'hiddeable_pan',
              mod_infoPanelOutput('mod_infoPanelOutput')
            )
          )
        ),
        
        ## cite div ####
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
  
  ## module calling ####
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
  infoPanel_reactives <- callModule(
    mod_infoPanel, 'mod_infoPanelOutput',
    data_reactives, map_reactives, viz_reactives
  )
  
  ## hide infoPanel ####
  observeEvent(
    eventExpr = {
      # all the inputs
      # data inputs
      data_reactives$ifn
      data_reactives$admin_divs
      data_reactives$espai_tipus
      data_reactives$apply_filters
      data_reactives$agg_level
      data_reactives$diam_class
      # viz inputs
      viz_reactives$color
      viz_reactives$mida
      viz_reactives$inverse_pal
      #
    },
    handlerExpr = {
      shinyjs::disable('hiddeable_pan')
      shinyjs::hide('hiddeable_pan')
    }
  )
  
  # observeEvent to showw the panel when a shape is clicked
  observeEvent(
    map_reactives$map_shape_click,
    {
      shinyjs::enable('hiddeable_pan')
      shinyjs::show('hiddeable_pan')
    }
  )
  
  ## debug #####
  output$debug1 <- renderPrint({
    data_reactives$agg_level
  })
  output$debug2 <- renderPrint({
    data_reactives$viz_shape
  })
  output$debug3 <- renderPrint({
    # infoPanel_reactives$data_shape() %>% collect() %>% as.data.frame() %>% head()
    data_reactives$data_core() %>% collect() %>% as.data.frame() %>% head()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

