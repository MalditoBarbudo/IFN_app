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
    absolutePanel(
      id = 'controls',  class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, top = 320, left = 20, right = 'auto', bottom = 'auto',
      width = 330, height = 'auto',
      
      # divs to each element, to be able to use shinyjs to play with them
      # ifn data selector
      div(
        id = 'ifn_sel',
        h2("Dades IFN"),
        
        selectInput(ns('ifn'), 'Versiò', ifns)
      ),
      
      # pinta y colorea selector
      div(
        id = 'colour_and_size',
        h2("Pinta y Colorea"),
        
        selectInput(
          ns('color'), 'Color', vars, selected = 'temperaturamitjanaanual'
        ),
        selectInput(ns('size'), 'Mida', vars, selected = 'Cap'),
        checkboxInput(ns('inverse_pal'), 'Invertir colors', value = FALSE)
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
  
  map_controls_reactives <- reactiveValues()
  
  # add here all the reactive expression that must be used for other modules o
  # the general app
  observe({
    map_controls_reactives$ifn <- input$ifn
    map_controls_reactives$color <- input$color
    map_controls_reactives$size <- input$size
    map_controls_reactives$inverse_pal <- input$inverse_pal
  })
  
  return(map_controls_reactives)
}
