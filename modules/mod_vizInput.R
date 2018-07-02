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
  observeEvent(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = {
      mod_data$agg_level
      mod_data$data_viz()
    },
    handlerExpr = {
      
      ## debug
      ## remove
      # browser()
      
      vars_clima <- names(mod_data$data_clima() %>% collect())
      vars_viz <- names(mod_data$data_viz() %>% collect())
      agg <- mod_data$agg_level
      grup_fun_val <- agg %>%
        stringr::str_remove('_rt') %>%
        stringr::str_remove('territori_') %>%
        paste0('id',.)
      
      # check if points or polygons
      if (agg %in% c(
        'parcela', 'especie', 'espsimple', 'genere', 'cadesclcon', 'plancon',
        'especie_rt', 'espsimple_rt', 'genere_rt', 'cadesclcon_rt', 'plancon_rt'
      )) {
        
        vars_to_use <- list(
          "Variables parcel·la" = vars_viz,
          "Variables climàtiques" = vars_clima
        )
        
        updateSelectInput(
          session, 'color', label = 'Color',
          choices = vars_to_use
        )
        
        # shinyjs::enable('mida')
        
        updateSelectInput(
          session, 'mida', label = 'Mida',
          choices = vars_to_use, selected = ''
        )
        
      } else {
        
        vars_to_use <- list(
          "Variables aggregació" = vars_viz
        )
        
        grup_func_choices <- mod_data$data_viz() %>% 
          # pull(!!sym(grup_fun_val))
          collect() %>%
          pull(!!sym(grup_fun_val))
        
        updateSelectInput(
          session, 'color', label = 'Color',
          choices = vars_to_use, selected = 'temperaturamitjanaanual'
        )
        
        # shinyjs::disable('mida')
        
        updateSelectInput(
          session, 'mida', label = grup_fun_val,
          choices = grup_func_choices
        )
        
      }
    }
  )
  # observe({
  #   
  #   vars_clima <- names(mod_data$data_clima() %>% collect())
  #   vars_viz <- names(mod_data$data_viz() %>% collect())
  #   agg <- mod_data$agg_level
  #   grup_fun_val <- agg %>%
  #     stringr::str_remove('_rt') %>%
  #     stringr::str_remove('territori_') %>%
  #     paste0('id',.)
  #   
  #   # check if points or polygons
  #   if (agg %in% c(
  #     'parcela', 'especie', 'espsimple', 'genere', 'cadesclcon', 'plancon',
  #     'especie_rt', 'espsimple_rt', 'genere_rt', 'cadesclcon_rt', 'plancon_rt'
  #   )) {
  #     
  #     vars_to_use <- list(
  #       "Variables parcel·la" = vars_viz,
  #       "Variables climàtiques" = vars_clima
  #     )
  #     
  #     updateSelectInput(
  #       session, 'color', label = 'Color',
  #       choices = vars_to_use, selected = 'temperaturamitjanaanual'
  #     )
  #     
  #     # shinyjs::enable('mida')
  #     
  #     updateSelectInput(
  #       session, 'mida', label = 'Mida',
  #       choices = vars_to_use, selected = ''
  #     )
  #     
  #   } else {
  #     
  #     vars_to_use <- list(
  #       "Variables aggregació" = vars_viz
  #     )
  #     
  #     grup_func_choices <- mod_data$data_viz() %>% 
  #       # pull(!!sym(grup_fun_val))
  #       collect() %>%
  #       pull(!!sym(grup_fun_val))
  #     
  #     updateSelectInput(
  #       session, 'color', label = 'Color',
  #       choices = vars_to_use, selected = 'temperaturamitjanaanual'
  #     )
  #     
  #     # shinyjs::disable('mida')
  #     
  #     updateSelectInput(
  #       session, 'mida', label = grup_fun_val,
  #       choices = grup_func_choices
  #     )
  #     
  #   }
  # })
  
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
