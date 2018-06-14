#' @title mod_mapControlsInput and mod_mapControls
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @export
mod_mapControlsInput <- function(id) {
  
  ns <- NS(id)
  
  ifns <- c(
    'IFN 2' = 'ifn2',
    'IFN 3' = 'ifn3'#,
    # 'IFN 4' = 'ifn4',
    
  )
  
  vars <- c(
    'Cap' = '',
    'Precipitació Anual' = 'precipitacioanual',
    'Temperatura Minima Anual' = 'temperaturaminimaanual',
    'Temperatura Mitjana Anual' = 'temperaturamitjanaanual',
    'Temperatura Maxima Anual' = 'temperaturamaximaanual',
    'Radiació Anual' = 'radiacioanual',
    'Altitud' = 'altitud',
    'Orientació' = 'orientacio',
    'Pendent (%)' = 'pendentpercentatge'
  )
  
  tagList(
    # divs to each element, to be able to use shinyjs to play with them
    # ifn data selector
    div(
      id = 'ifn_sel',
      selectInput(ns('ifn'), 'Versiò de les dades', ifns, selected = 'ifn2')
    ),
    
    # administrative divisions
    div(
      id = 'admin_divs',
      selectInput(
        ns('territori'), 'Divisions administratives',
        choices = c(
          Provincies = 'provincia', Vegueries = 'vegueria',
          Comarques = 'comarca', Municipis = 'municipi'
        ),
        selected = 'provincia'
      )
      
      # radioButtons(
      #   ns('territori'), '',
      #   choices = c(
      #     Provincies = 'provincia', Vegueries = 'vegueria',
      #     Comarques = 'comarca', Municipis = 'municipi'
      #   ),
      #   inline = TRUE, width = '80%',
      #   selected = 'provincia'
      # )
    ),
    
    # pinta y colorea selector
    div(
      id = 'colour_and_size',
      selectInput(
        ns('color'), 'Color de les parcel·les', vars, selected = 'temperaturamitjanaanual'
      ),
      checkboxInput(ns('inverse_pal'), 'Invertir colors', value = FALSE),
      selectInput(ns('size'), 'Mida de les parcel·les', vars, selected = 'Cap')
    ),
    
    # show/hide selection panel button
    div(
      id = 'controls_button',
      p('Mostrar les eines de selecció de parcel·les'),
      actionButton(
        ns('show_sel'), 'Mostra / Amaga'
      )
    )
  )
  
}

#' mod_mapControls server function
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @export
#' 
#' @rdname mod_mapControls
mod_mapControls <- function(
  input, output, session
) {
  
  mapControls_reactives <- reactiveValues()
  
  # add here all the reactive expression that must be used for other modules o
  # the general app
  observe({
    mapControls_reactives$ifn <- input$ifn
    mapControls_reactives$color <- input$color
    mapControls_reactives$size <- input$size
    mapControls_reactives$inverse_pal <- input$inverse_pal
    mapControls_reactives$territori <- input$territori
    mapControls_reactives$show_sel <- input$show_sel
  })
  
  # return the 
  return(mapControls_reactives)
}
