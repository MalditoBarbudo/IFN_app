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
        ),
        disabled(
          selectInput(
            ns('grup_func'), 'Grup funcional', c(Cap = ''), width = '100%'
          )
        ),
        disabled(
          selectInput(
            ns('statistic'), 'Mètrica', c(Cap = ''), width = '100%'
          )
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
  
  # update inputs with variables present in data. We have four input scenarios
  # so we build a reactive to know which scenario we have
  input_scenario <- reactive({
    if (mod_data$viz_shape == 'parcela') {
      if (mod_data$agg_level == 'parcela') {
        return('scenario1')
      } else {
        return('scenario2')
      }
    } else {
      if (mod_data$agg_level == 'parcela') {
        return('scenario3')
      } else {
        return('scenario4')
      }
    }
  })
  
  # and now an observeEvent based on scenario
  observeEvent(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = {
      input_scenario()
    },
    handlerExpr = {
      
      scenario <- input_scenario()
      
      if (scenario == 'scenario1') {
        
        # data needed
        vars_clima <- names(mod_data$data_clima() %>% collect()) %>%
          stringr::str_sort()
        vars_viz <- names(mod_data$data_core()) %>%
          stringr::str_sort()
        
        vars_to_use <- list(
          "Variables parcel·la" = vars_viz,
          "Variables climàtiques" = vars_clima
        )
        
        # update the needed inputs
        updateSelectInput(
          session, 'color', label = 'Color',
          choices = vars_to_use
        )
        
        updateSelectInput(
          session, 'mida', label = 'Mida',
          choices = vars_to_use, selected = ''
        )
        
        # enable/disable and show/hide the needed inputs
        shinyjs::enable('mida')
        shinyjs::disable('grup_func')
        shinyjs::disable('statistic')
        shinyjs::reset('grup_func')
        shinyjs::reset('statistic')
        
      } else {
        if (scenario == 'scenario2') {
          
          # data needed
          vars_clima <- names(mod_data$data_clima() %>% collect()) %>%
            stringr::str_sort()
          vars_viz <- names(mod_data$data_core()) %>%
            stringr::str_sort()
          
          vars_to_use <- list(
            "Variables parcel·la" = vars_viz,
            "Variables climàtiques" = vars_clima
          )
          
          grup_func_var <- glue('id{mod_data$agg_level}')
          
          grup_func_choices <- mod_data$data_core() %>%
            pull(!!sym(grup_func_var)) %>%
            stringr::str_sort()
          
          # update the needed inputs
          updateSelectInput(
            session, 'color', label = 'Color',
            choices = vars_to_use
          )
          
          updateSelectInput(
            session, 'grup_func', label = glue('{mod_data$agg_level}'),
            choices = grup_func_choices
          )
          
          # enable/disable and show/hide the needed inputs
          shinyjs::enable('grup_func')
          shinyjs::disable('statistic')
          shinyjs::reset('statistic')
          
        } else {
          if (scenario == 'scenario3') {
            # data needed
            vars_viz <- names(mod_data$data_core()) %>%
              stringr::str_sort() %>% 
              stringr::str_remove(
                pattern = '_mean$|_sd$|_min$|_max$|_n$|_q95$|_median$'
              ) %>%
              unique()
            
            vars_to_use <- list(
              "Variables parcel·la" = vars_viz
            )
            
            statistics_choices <- c(
              'Mitjana' = '_mean', 'Mediana' = '_median',
              'Desviació estàndard' = '_sd', 'Mìnim' = '_min', 'Màxim' = '_max',
              'Nombre parcel·les' = '_n', 'Quartil 95' = '_q95'
            )
            
            # update the needed inputs
            updateSelectInput(
              session, 'color', label = 'Color',
              choices = vars_to_use
            )
            
            updateSelectInput(
              session, 'statistic', label = 'Mètrica',
              choices = statistics_choices
            )
            
            # enable/disable and show/hide the needed inputs
            shinyjs::disable('grup_func')
            shinyjs::disable('mida')
            shinyjs::enable('statistic')
            shinyjs::reset('grup_func')
            shinyjs::reset('mida')
            
          } else {
            # scenario4
            # data needed
            vars_viz <- names(mod_data$data_core()) %>%
              stringr::str_sort() %>% 
              stringr::str_remove(
                pattern = '_mean$|_sd$|_min$|_max$|_n$|_q95$|_median$'
              ) %>%
              unique()
            
            vars_to_use <- list(
              "Variables parcel·la" = vars_viz
            )
            
            statistics_choices <- c(
              'Mitjana' = '_mean', 'Mediana' = '_median',
              'Desviació estàndard' = '_sd', 'Mìnim' = '_min', 'Màxim' = '_max',
              'Nombre parcel·les' = '_n', 'Quartil 95' = '_q95'
            )
            
            grup_func_var <- glue('id{mod_data$agg_level}')
            
            grup_func_choices <- mod_data$data_core() %>%
              pull(!!sym(grup_func_var)) %>%
              stringr::str_sort()
            
            # update the needed inputs
            updateSelectInput(
              session, 'color', label = 'Color',
              choices = vars_to_use
            )
            
            updateSelectInput(
              session, 'statistic', label = 'Mètrica',
              choices = statistics_choices
            )
            
            updateSelectInput(
              session, 'grup_func', label = glue('{mod_data$agg_level}'),
              choices = grup_func_choices
            )
            
            # enable/disable and show/hide the needed inputs
            shinyjs::enable('grup_func')
            shinyjs::enable('statistic')
            shinyjs::disable('mida')
            shinyjs::reset('mida')
            
          }
        }
      }
    }
  )
  
  # reactive with the inputs values
  viz_reactives <- reactiveValues()
  
  observe({
    # inputs
    viz_reactives$color <- input$color
    viz_reactives$inverse_pal <- input$inverse_pal
    viz_reactives$mida <- input$mida
    viz_reactives$grup_func <- input$grup_func
    viz_reactives$statistic <- input$statistic
  })
  
  return(viz_reactives)
}
