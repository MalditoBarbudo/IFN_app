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
   fluidRow(
     # administrative divisions
     column(
       6,
       div(
         id = 'admin_divs',
         selectInput(
           ns('admin_divs'), '', 'Totes', selected = 'Totes', multiple = TRUE
         )
       ),
       div(
         id = 'proteccio_divs',
         selectInput(
           ns('proteccio_divs'), 'Filtrar por nivell de protecció',
           c(
             "Tots",
             sort(
               c(
                 "Parc Natural", "Paratge Natural d'Interès Nacional",
                 "Sense protecció", "Reserva Natural Parcial", "Zona de Protecció",
                 "Parc Nacional", "Reserva Natural de Fauna Salvatge"
               )
             )
           ),
           selected = 'Tots', multiple = TRUE
         )
       )
     ),
     column(
       6,
       br(),
       div(
         id = 'fil_btns', inline = TRUE,
         actionButton(ns('apply_btn'), 'Aplicar filtres')
         # actionButton(ns('clear_btn'), 'Reset filtre')
       )
     )
     
   ),
   
   div(
     id = 'input2',
     textOutput(ns('debug_fil'))
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
        provincia = '', vegueria = '', comarca = '', municipi = ''
      )
    } else {
      input_choices <- noms
    }
    
    # update the input
    updateSelectInput(
      session, "admin_divs",
      label = paste0('Filtra per ', territori),
      choices = input_choices[[territori]],
      selected = input_choices[[territori]][1]
    )
  })
  
  # reactive values to return from the module
  filterAndSel_reactives <- reactiveValues()
  
  observe({
    filterAndSel_reactives$apply_btn <- input$apply_btn
    # filterAndSel_reactives$clear_btn <- input$clear_btn
    filterAndSel_reactives$admin_divs <- input$admin_divs
    filterAndSel_reactives$proteccio_divs <- input$proteccio_divs
  })
  
  # debug output
  output$debug_fil <- renderPrint({
    filterAndSel_reactives$proteccio_divs
  })
  
  return(filterAndSel_reactives)
 
}


