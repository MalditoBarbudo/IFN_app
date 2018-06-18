#' @title mod_sigTableReactiveOutput and mod_sigTableReactive
#' 
#' @description A shiny module to generate the sig table
#' 
#' @param id shiny id
#' 
#' @export
mod_sigTableReactiveOutput <- function(id) {
  ns <- NS(id)
}

#' mod_sigTableReactive server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param map_controls reactive input from mapControls module
#' @param filterAndSel_inputs reactives from mod_filterAndSel module
#' 
#' @export
#' 
#' @rdname mod_sigTableReactiveOutput
mod_sigTableReactive <- function(
  input, output, session,
  map_controls, filterAndSel_inputs
) {
  
  # reactive for sig table
  sig_tbl_call <- reactive({
    
    # inputs
    ifn <- map_controls$ifn
    # name
    sig_name <- paste0('parcela', ifn, '_sig_etrs89')
    # table
    tbl(oracle_ifn, sig_name)
    
  })
  
  return(sig_tbl_call)
  
}
