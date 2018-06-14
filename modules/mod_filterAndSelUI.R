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
   
   # two rows, one for administrative divisions selection and another one for
   # the protection levels selection
   
   # 1st row, full width to avoid growing in height whenever is possible
   fluidRow(
     column(
       12,
       selectInput(
         ns('admin_divs'), '', 'Totes', selected = 'Totes', multiple = TRUE,
         width = '100%'
       )
     )
   ),
   
   # 2nd rowm two columns, the right one as an input updated depending on the
   # selection in the left one
   fluidRow(
     column(
       5,
       selectInput(
         ns('proteccion_divs'), "Tipus d'espai protegit",
         choices = c(
           'Nivell de protecció' = 'proteccio',
           "Espai d'interès Nacional" = 'nomein',
           "Espai de protecció especial" = 'enpes',
           "Xarxa Natura 2000" = 'nomxarxa2000'
         )
       )
     ),
     
     column(
       7,
       selectInput(
         ns('proteccion_levels'), '',
         'Totes', selected = 'Totes', multiple = TRUE, width = '100%'
       )
     )
   ),
   # div(
   #   id = 'input2',
   #   textOutput(ns('debug_fil'))
   # ),
   div(
     id = 'fil_btns', inline = TRUE,
     actionButton(ns('apply_btn'), 'Aplicar filtres')
     # actionButton(ns('clear_btn'), 'Reset filtre')
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
  
  # observer for update the proteccion levels
  observe({
    # get the protection division and create the choices based on the dic
    tipo_espai <- input$proteccion_divs
    choices <- proteccion_dictionary[[tipo_espai]]
    
    # update the input
    updateSelectInput(
      session, 'proteccion_levels',
      label = paste0(" Filtra per ", tipo_espai),
      choices = choices,
      selected = choices[1]
    )
    
  })
  
  # reactive values to return from the module
  filterAndSel_reactives <- reactiveValues()
  
  observe({
    filterAndSel_reactives$apply_btn <- input$apply_btn
    # filterAndSel_reactives$clear_btn <- input$clear_btn
    filterAndSel_reactives$admin_divs <- input$admin_divs
    filterAndSel_reactives$proteccion_levels <- input$proteccion_levels
    filterAndSel_reactives$proteccion_divs <- input$proteccion_divs
  })
  
  # debug output
  # output$debug_fil <- renderPrint({
  #   filterAndSel_reactives$proteccion_divs
  # })
  
  return(filterAndSel_reactives)
 
}


