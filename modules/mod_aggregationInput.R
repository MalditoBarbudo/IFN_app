#' @title mod_aggregationInput and mod_aggregation
#' 
#' @description A shiny module to generate the base IFN plots table
#' 
#' @param id shiny id
#' 
#' @export
mod_aggregationInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(
        7,
        selectInput(
          ns('aggregation_level'), "Nivell d'agregació",
          choices = list(
            
            "Parcel·les" = list(
              'Parcel·la' = 'parcela',
              'Parcel·la / Espècie' = 'especie',
              'Parcel·la / Espècie simplificat' = 'espsimple',
              'Parcel·la / Gènere' = 'genere',
              'Parcel·la / Tipu funcional' = 'cadesclcon',
              'Parcel·la / Tipu funcional simplificat' = 'plancon'
            ),
            
            "Grups funcionals" = list(
              'Espècie' = 'especie_rt',
              'Espècie simplificat' = 'espsimple_rt',
              'Gènere' = 'genere_rt',
              'Tipu funcional' = 'cadesclcon_rt',
              'Tipu funcional simplificat' = 'plancon_rt'
            ),
            'Administratiu' = list("Divisions seleccionats" = 'territori_rt')
          ), selected = ''
        ),
        
        checkboxInput(
          ns('diameter_classes'), '¿Afegir per classes diametriques?',
          value = FALSE
        )
      ),
      
      column(
        5,
        br(),
        wellPanel(
          actionButton(
            'activate_table', 'Explora les dades',
            icon = icon('search'), width = '100%'
          )
        )
      )
    )
  )
}

#' mod_aggregation server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mapControls reactives with the inputs from the map module
#' 
#' 
#' @export
#' 
#' @rdname mod_aggregation
mod_aggregation <- function(
  input, output, session,
  mapControls
) {
  
  
  # observe, collect and return the inputs
  aggregation_inputs <- reactiveValues()

  observe({

    aggregation_inputs$aggregation_level <- input$aggregation_level
    aggregation_inputs$diameter_classes <- input$diameter_classes
    aggregation_inputs$activate_table <- input$activate_table

  })

  return(aggregation_inputs)
}
