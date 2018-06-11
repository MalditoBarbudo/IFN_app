#' @title mod_filterAndSelUI and mod_filterAndSel
#' 
#' @description A shiny module to filter and select the data for further
#'  analysis
#' 
#' @param id shiny id
#' 
#' @export
mod_filterAndSelUI <- function(id) {
 
 ns <- NS(id)
 
 tagList(
  # we create several divs with id to work with shinyjs in case we need it
  
  # administrative divisions
  div(
   id = 'admin_divs',
   selectInput(
    ns('admin_divs'), '', 'Totes', selected = 'Totes', multiple = TRUE
   )
  ),
  
  div(
    id = 
  )
  
 )
 
}

#' mod_filterAndSel server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param control_inputs Reactive with the inputs from mapControlsInput module
#' @param noms List with the admin divs names
#' 
#' @export
#' 
#' @rdname mod_filterAndSel
mod_filterAndSel <- function(
 input, output, session,
 control_inputs, noms
) {
 
 # observer for update the admin_divs input
 observe({
  
  # create the input choices based on the territori input
  territori <- control_inputs$territori
  if (is.null(territori)) {
   input_choices <- list(
     provincies = '', vegueries = '', comarques = '', municipis = ''
   )
  } else {
   input_choices <- noms
  }
  
  # update the input
  updateSelectInput(
   session, "admin_divs",
   label = paste0('Filtra per ', input_choices[[territori]])
  )
 })
 
}


