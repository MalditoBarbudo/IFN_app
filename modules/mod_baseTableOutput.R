#' @title mod_baseTableOutput and mod_baseTable
#' 
#' @description A shiny module to generate the base IFN plots table
#' 
#' @param id shiny id
#' 
#' @export
mod_baseTableOutput <- function(id) {
  
  ns <- NS(id)
  
  # What we need here?
  # We need two rows, the second one with two columns for three tables in
  # total, the data table (1st row) and the filtering tables (2nd row) which will
  # be used to filter sites by some things
  tagList(
    
    # first row, data table
    # fluidRow(
    #   column(
    #     12,
    #     DTOutput(ns('table_dades'), height = 350)
    #   )
    # )#,
    
    DTOutput(ns('table_dades'))
    
    # second row, filtering tables
    # fluidRow(
    #   column(
    #     6,
    #     DTOutput(ns('table_filtering_one'))
    #   ),
    #   column(
    #     6,
    #     DTOutput(ns('table_filtering_two')),
    #     textOutput(ns('debug_tables'))
    #   )
    # )
  )
  
}

#' mod_baseTable server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mapControls reactives with the inputs from the mapControls module
#' @param tableControls reactives with the inputs from the tableControls inputs
#' @param dataReactive reactive data with the map points 
#' 
#' 
#' @export
#' 
#' @rdname mod_baseTable
mod_baseTable <- function(
  input, output, session,
  mapControls, tableControls, dataReactive
) {
  
  # retrieve table, based on selections
  table_dades <- reactive({
    table_name <- tableControls$table_name()
    id_var <- tableControls$id_var()
    tipus_selector <- tableControls$tipus_selector
    filter_arg <- quo(!! id_var %in% tipus_selector)
    
    table_dades <- tbl(oracle_ifn, table_name) %>%
      filter(!!! filter_arg)
  })
  
  # tabla dades
  output$table_dades <- renderDT(
    server = TRUE,
    expr = {
      
      table_dades() %>%
        collect() %>%
        datatable(
          filter = list(position = 'top', clear = TRUE, plain = FALSE),
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          extensions = c('Buttons', 'Scroller'),
          options = list(
            dom = 'tB',
            extend = 'collection',
            buttons = c('csv', 'pdf'),
            text = 'Descàrrega',
            autoWidth = TRUE,
            deferRender = TRUE, scrollY = 250, scroller = TRUE
          )
        )
    }
  )
  
  
  # debug!!!
  # renderPrint(
  #   list(table_name, id_var, tipus_selector)
  # )
}
