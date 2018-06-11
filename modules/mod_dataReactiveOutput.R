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
#' @param map_controls reactive input from mapControls module
#' @param filterAndSel_inputs reactives from mod_filterAndSel module
#' 
#' @export
#' 
#' @rdname mod_dataReactiveOutput
mod_dataReactive <- function(
  input, output, session,
  map_controls, filterAndSel_inputs = NULL
) {
  
  # data depending on several events
  data_parcelas <- eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = {
      # if (
      #   isTruthy(map_controls$ifn) ||
      #   isTruthy(map_controls$color) ||
      #   isTruthy(map_controls$size) ||
      #   isTruthy(map_controls$inverse_pal) ||
      #   isTruthy(filterAndSel_inputs$apply_btn)
      # ) {
      #   TRUE
      # } else {
      #   return()
      # }
      
      map_controls$ifn
      map_controls$color
      map_controls$size
      map_controls$inverse_pal
      filterAndSel_inputs$apply_btn
      
    },
    
    valueExpr = {
      
      ifn_sel <- map_controls$ifn
      # tables names, depending on the ifn selected
      clima_name <- paste0('parcela', ifn_sel, '_clima')
      sig_name <- paste0('parcela', ifn_sel, '_sig_etrs89')
      cec_name <- paste0('r_cadesclcon_', ifn_sel)
      # table for
      #   1. parcelas
      #   2. colores y tamaños
      #   3. popups
      data_par <- tbl(oracle_ifn, clima_name) %>%
        inner_join(tbl(oracle_ifn, sig_name), by = 'idparcela') %>%
        inner_join(tbl(oracle_ifn, cec_name), by = 'idparcela')
      
      if (filterAndSel_inputs$apply_btn == 0) {
        return(data_par %>% collect())
      } else {
        
        # return all data by default or when totes is selected
        territoris <- isolate(filterAndSel_inputs$admin_divs)
        proteccio_fils <- isolate(filterAndSel_inputs$proteccio_divs)
        
        if (is.null(territoris)) {
          return(data_par %>% collect())
        } else {
          if (any(territoris %in% c('Totes', 'Tots'))) {
            return(data_par %>% collect())
          } else {
            # filter if only some territorises are selected
            if (isolate(map_controls$territori) == 'provincia') {
              return(
                data_par %>%
                  filter(provincia %in% territoris &&
                           proteccio %in% proteccio_fils) %>%
                  collect()
              )
            }
            
            if (isolate(map_controls$territori) == 'vegueria') {
              return(data_par %>%
                       filter(vegueria %in% territoris) %>%
                       collect())
            }
            
            if (isolate(map_controls$territori) == 'comarca') {
              return(
                data_par %>%
                  filter(comarca %in% territoris) %>%
                  collect()
              )
            }
            
            if (isolate(map_controls$territori) == 'municipi') {
              return(
                data_par %>%
                  filter(municipi %in% territoris) %>%
                  collect()
              )
            }
          }
        }
      }
    }
  )
  
  
  # data
  # data_parcelas <- reactive({
  #   
  #   ifn_sel <- map_controls$ifn
  #   territori <- map_controls$territori
  #   
  #   # tables names, depending on the ifn selected
  #   clima_name <- paste0('parcela', ifn_sel, '_clima')
  #   sig_name <- paste0('parcela', ifn_sel, '_sig_etrs89')
  #   cec_name <- paste0('r_cadesclcon_', ifn_sel)
  #   
  #   # table for
  #   #   1. parcelas
  #   #   2. colores y tamaños
  #   #   3. popups
  #   data_par <- tbl(oracle_ifn, clima_name) %>%
  #     inner_join(tbl(oracle_ifn, sig_name), by = 'idparcela') %>%
  #     inner_join(tbl(oracle_ifn, cec_name), by = 'idparcela') #%>%
  #     # collect()
  #   
  #   
  #   
  # })
  
  return(data_parcelas)
  
}
