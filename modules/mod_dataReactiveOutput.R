#' @title mod_dataReactiveOutput and mod_dataReactive
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @export
mod_dataReactiveOutput <- function(id) {
  ns <- NS(id)
}

#' mod_dataReactive server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param ifn reactive input indicating the data origin (ifn)
#' 
#' @export
#' 
#' @rdname mod_dataReactiveOutput
mod_dataReactive <- function(
  input, output, session,
  ifn
) {
  
  # data
  data_parcelas <- reactive({
    
    ifn_sel <- ifn()
    
    # tables names, depending on the ifn selected
    clima_name <- paste0('parcela', ifn_sel, '_clima')
    sig_name <- paste0('parcela', ifn_sel, '_sig_etrs89')
    cec_name <- paste0('r_cadesclcon_', ifn_sel)
    
    # table for
    #   1. parcelas
    #   2. colores y tamaños
    #   3. popups
    tbl(oracle_ifn, clima_name) %>%
      inner_join(tbl(oracle_ifn, sig_name), by = 'idparcela') %>%
      inner_join(tbl(oracle_ifn, cec_name), by = 'idparcela') %>%
      # select(idparcela) %>%
      collect()
  })
  
  return(data_parcelas)
  
}
