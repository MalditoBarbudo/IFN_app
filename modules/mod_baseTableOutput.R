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
#' @param aggregation reactives with the inputs from the aggregation module
#' @param dataReactive reactive data with the map points 
#' @param filterAndSel reactive with the inputs from filterAndSel module
#' 
#' 
#' @export
#' 
#' @rdname mod_baseTable
mod_baseTable <- function(
  input, output, session,
  mapControls, aggregation, dataReactive, filterAndSel, parcelas
) {
  
  # reactive for table name creation
  table_name <- reactive({
    
    # inputs
    ifn <- mapControls$ifn
    agg <- aggregation$aggregation_level
    cd <- if (aggregation$diameter_classes) {'cd'} else {''}
    
    # real time calculations TODO!!
    if (stringr::str_detect(agg, '_rt')) {
      return() # TODO
    } else {
      
      # parcela (especial case, as there is no aggregation level)
      if (agg == 'parcela') {
        table_name <- paste0('r_', cd, ifn)
        return(table_name)
      } else {
        table_name <- paste0('r_', agg, cd, '_', ifn)
        return(table_name)
      }
    }
  })
  
  # reactive for table data
  tbl_call <- reactive({
    tbl(oracle_ifn, table_name())
  })
  
  # DT render
  output$table_dades <- renderDT(
    server = TRUE,
    expr = {
      
      parcelas <- dataReactive() %>%
        pull('idparcela')
      
      tbl_call() %>%
        filter(idparcela %in% parcelas) %>%
        collect() %>%
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
            deferRender = TRUE, scrollY = 450, scroller = TRUE
          )
        )
    }
  )
  
  
  # reactiveValues to return
  baseTable_reactives <- reactiveValues()
  
  observe({
    
    baseTable_reactives$table_name <- table_name
    
  })
  
  return(baseTable_reactives)
  
  
  
  # # retrieve table, based on selections
  # table_dades <- reactive({
  #   table_name <- tableControls$table_name()
  #   id_var <- tableControls$id_var()
  #   tipus_selector <- tableControls$tipus_selector
  #   filter_arg <- quo(!! id_var %in% tipus_selector)
  #   
  #   table_dades <- tbl(oracle_ifn, table_name) %>%
  #     filter(!!! filter_arg)
  # })
  # 
  # # tabla dades
  # output$table_dades <- renderDT(
  #   server = TRUE,
  #   expr = {
  #     
  #     table_dades() %>%
  #       collect() %>%
  #       datatable(
  #         filter = list(position = 'top', clear = TRUE, plain = FALSE),
  #         style = 'default', rownames = FALSE,
  #         fillContainer = TRUE, autoHideNavigation = TRUE,
  #         extensions = c('Buttons', 'Scroller'),
  #         options = list(
  #           dom = 'tB',
  #           extend = 'collection',
  #           buttons = c('csv', 'pdf'),
  #           text = 'Descàrrega',
  #           autoWidth = TRUE,
  #           deferRender = TRUE, scrollY = 250, scroller = TRUE
  #         )
  #       )
  #   }
  # )
  
  
  # debug!!!
  # renderPrint(
  #   list(table_name, id_var, tipus_selector)
  # )
}
