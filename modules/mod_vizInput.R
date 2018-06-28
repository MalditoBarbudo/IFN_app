#' @title mod_vizInput and mod_viz
#' 
#' @description A shiny module to generate the data tables
#' 
#' @param id shiny id
#' 
#' @export
mod_vizInput <- function(id) {
  
  # ns
  ns <- NS(id)
  
  # UI
  tagList(
    
    # div and id is for later use of shinyjs. Inputs will be empty and
    # populated later on with the data in the server side
    div(
      id = 'vizInputs',
      
      wellPanel(
        selectInput(
          ns('color'), 'Color', c(Cap = ''), width = '100%'
        ),
        checkboxInput(
          ns('inverse_pal'), 'Invertir colors', value = FALSE
        ),
        selectInput(
          ns('mida'), 'Mida', c(Cap = ''), width = '100%'
        )
      )
    )
  )
}

#' mod_viz server function
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @param mod_data reactive with the reactive data and the data inputs
#' 
#' @export
#' 
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  mod_data
) {
  
  # update inputs with variables present in data
  observe({
    
    vars_clima <- names(mod_data$data_clima() %>% collect())
    vars_core <- names(mod_data$data_viz() %>% collect())
    
    vars_to_use <- list(
      "Variables parcel·la" = vars_core,
      "Variables climàtiques" = vars_clima
    )
    
    updateSelectInput(
      session, 'color', label = 'Color',
      choices = vars_to_use, selected = 'temperaturamitjanaanual'
    )
    
    updateSelectInput(
      session, 'mida', label = 'Mida',
      choices = vars_to_use, selected = ''
    )
    
  })
  
  # reactive with the inputs values
  viz_reactives <- reactiveValues()
  
  observe({
    # inputs
    viz_reactives$color <- input$color
    viz_reactives$inverse_pal <- input$inverse_pal
    viz_reactives$mida <- input$mida
  })
  
  return(viz_reactives)
}
