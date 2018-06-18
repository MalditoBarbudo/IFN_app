#' @title mod_climaTableReactiveOutput and mod_climaTableReactive
#' 
#' @description A shiny module to generate the clima table
#' 
#' @param id shiny id
#' 
#' @export
mod_climaTableReactiveOutput <- function(id) {
  ns <- NS(id)
}

#' mod_climaTableReactive server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param map_controls reactive input from map_controls module
#' @param filterAndSel_inputs reactives from mod_filterAndSel module
#' 
#' @export
#' 
#' @rdname mod_climaTableReactiveOutput
mod_climaTableReactive <- function(
  input, output, session,
  map_controls, filterAndSel_inputs
) {
  
  # reactive for clima table
  clima_tbl_call <- reactive({
    
    # inputs
    ifn <- map_controls$ifn
    # name
    clima_name <- paste0('parcela', ifn, '_clima')
    # table
    tbl(oracle_ifn, clima_name)
    
  })
  
  return(clima_tbl_call)
  
}
