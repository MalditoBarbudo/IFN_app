#' @title mod_dataTableReactiveOutput and mod_dataTableReactive
#' 
#' @description A shiny module to generate the clima table
#' 
#' @param id shiny id
#' 
#' @export
mod_dataTableReactiveOutput <- function(id) {
  ns <- NS(id)
}

#' mod_dataTableReactive server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param map_controls reactive input from mapControls module
#' @param filterAndSel_inputs reactives from mod_filterAndSel module
#' @param aggregation reactives from mod_aggregation module
#' 
#' @export
#' 
#' @rdname mod_dataTableReactiveOutput
mod_dataTableReactive <- function(
  input, output, session,
  map_controls, filterAndSel_inputs, aggregation
) {
  
  # reactive for data table
  data_tbl_call <- reactive({
    
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
        if (cd == '') {
          table_name <- paste0('r_', ifn)
        } else {
          table_name <- paste0('r_', cd, '_', ifn)
        }
      } else {
        table_name <- paste0('r_', agg, cd, '_', ifn)
      }
    }
    
    tbl(oracle_ifn, table_name)
    
  })
  
  return(data_tbl_call)
  
}
