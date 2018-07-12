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
        h3('Visualització'),
        selectInput(
          ns('color'), 'Color', c(Cap = ''), width = '100%'
        ),
        checkboxInput(
          ns('inverse_pal'), 'Invertir colors', value = FALSE
        ),
        selectInput(
          ns('mida'), 'Mida', c(Cap = ''), width = '100%'
        ),
        selectInput(
          ns('tipo_grup_func'), 'Tipus grup funcional',
          choices = c(
            'Espècie' = 'especie',
            'Espècie simplificat' = 'especiesimple',
            'Gènere' = 'genere',
            'Conífera/Caducifoli/Esclerofil·le' = 'caducesclerconif',
            'Conífera/Planifoli' = 'planifconif'
          ), width = '100%'
        ),  
        selectInput(
          ns('grup_func'), 'Grup funcional', c(Cap = ''), width = '100%'
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
  # so we build a reactive to know which scenario we have using the get_scenario
  # function from global.R
  input_scenario <- reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
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
        
        # reset needed inputs and hide no needed inputs
        shinyjs::reset('color')
        shinyjs::reset('mida')
        shinyjs::reset('tipo_grup_func')
        shinyjs::reset('grup_func')
        shinyjs::hide('statistic')
        shinyjs::show('mida')
        shinyjs::show('tipo_grup_func')
        shinyjs::show('grup_func')
        shinyjs::enable('mida')
        shinyjs::enable('tipo_grup_func')
        shinyjs::enable('grup_func')
        
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
        
        grup_func_choices <- mod_data$data_core() %>%
          pull(!!sym(glue('{input$tipo_grup_func}dens'))) %>%
          stringr::str_sort() %>%
          c('Qualsevol', .)

        updateSelectInput(
          session, 'grup_func', label = glue('{input$tipo_grup_func} dominant per densitat'),
          choices = grup_func_choices, selected = 'Qualsevol'
        )
        
      } else {
        if (scenario == 'scenario2') {
          
          # reset needed inputs and hide no needed inputs
          shinyjs::reset('color')
          shinyjs::reset('mida')
          shinyjs::reset('grup_func')
          shinyjs::hide('statistic')
          shinyjs::hide('tipo_grup_func')
          shinyjs::show('mida')
          shinyjs::show('grup_func')
          shinyjs::enable('mida')
          shinyjs::enable('grup_func')
          
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
            session, 'mida', label = 'Mida',
            choices = vars_to_use, selected = ''
          )
          
          updateSelectInput(
            session, 'grup_func', label = glue('{mod_data$agg_level}'),
            choices = grup_func_choices
          )
          
        } else {
          if (scenario == 'scenario3') {
            
            # reset needed inputs and hide no needed inputs
            shinyjs::reset('color')
            shinyjs::reset('tipo_grup_func')
            shinyjs::reset('grup_func')
            shinyjs::reset('statistic')
            shinyjs::hide('mida')
            shinyjs::show('tipo_grup_func')
            shinyjs::show('grup_func')
            shinyjs::show('statistic')
            shinyjs::enable('tipo_grup_func')
            shinyjs::enable('grup_func')
            shinyjs::enable('statistic')
            
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
            
            # grup_func_choices <- mod_data$data_core() %>%
            #   pull(!!sym(glue('{input$tipo_grup_func}dens'))) %>%
            #   stringr::str_sort()
            grup_func_choices <- data_generator(
              oracle_ifn, mod_data$ifn, 'parcela', 'parcela', NULL, FALSE,
              {mod_data$data_sig() %>% collect()}, NULL
            ) %>%
              pull(!!sym(glue('{input$tipo_grup_func}dens'))) %>%
              stringr::str_sort() %>%
              c('Qualsevol', .)
              
            
            updateSelectInput(
              session, 'grup_func',
              label = glue('{input$tipo_grup_func} dominant per densitat'),
              choices = grup_func_choices
            )
            
          } else {
            # scenario4
            
            # reset needed inputs and hide no needed inputs
            shinyjs::reset('color')
            shinyjs::reset('grup_func')
            shinyjs::reset('statistic')
            shinyjs::hide('tipo_grup_func')
            shinyjs::hide('mida')
            shinyjs::show('color')
            shinyjs::show('grup_func')
            shinyjs::show('statistic')
            shinyjs::enable('color')
            shinyjs::enable('grup_func')
            shinyjs::enable('statistic')
            
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
