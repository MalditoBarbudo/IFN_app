#' @title mod_tableOutput and mod_table
#' 
#' @description A shiny module to generate the base IFN plots table
#' 
#' @param id shiny id
#' 
#' @export
mod_tableOutput <- function(id) {
  
  # ns
  ns <- NS(id)
  
  # UI
  tagList(
    
    fluidRow(
      column(
        12,
        DTOutput(ns('core_table'))
      )
    )
  )
}

#' mod_table server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mod_data reactive with the reactive data and the data inputs
#' 
#' @export
#' 
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  mod_data
) {
  
  output$core_table <- renderDT(
    server = TRUE,
    expr = {
      mod_data$data_core() %>% 
        datatable(
          filter = list(position = 'top', clear = TRUE, plain = FALSE),
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          extensions = c('Buttons', 'Scroller'),
          options = list(
            dom = 'tBi',
            extend = 'collection',
            buttons = c('csv', 'colvis'),
            text = 'Descàrrega',
            autoWidth = TRUE,
            deferRender = TRUE, scrollY = '70vh', scroller = TRUE
          )
        )
    }
  )
  
}
