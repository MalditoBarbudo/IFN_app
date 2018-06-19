#' @title mod_infoPanelOutput and mod_infoPanel
#' 
#' @description A shiny module to generate the data tables
#' 
#' @param id shiny id
#' 
#' @export
mod_infoPanelOutput <- function(id) {
  
  # ns
  ns <- NS(id)
  
  # UI outputs
  tagList(
    absolutePanel(
      # panel settings
      id = 'infoPanel', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 640, height = 'auto',
      top = 'auto', left = 'auto', bottom = 0, right = 15,
      
      # panel contents
      tabsetPanel(
        id = 'infoPanel_tabs', type = 'pills',
        
        tabPanel(
          'Map click',
          uiOutput(ns('shape_click_info')),
          br(),
          plotOutput(ns('shape_click_plot'), width = 540),
          br(),
          textOutput(ns('infoPanel_debug'))
        ),
        
        tabPanel(
          'General info',
          'Que va aqui????'
        )
      )
    )
  )
}
