#' @title mod_dataUI and mod_data
#' 
#' @description A shiny module to generate the data tables
#' 
#' @param id shiny id
#' 
#' @export
mod_dataUI <- function(id) {
  
  # ns
  ns <- NS(id)
  
  # UI
  tagList(
    
    # absolute panel for all, later on we will be able to hide/show the different
    # parts of the panel
    absolutePanel(
      # panel settings
      id = 'dataControls', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 640, height = 'auto',
      # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
      # top = 'auto', left = 'auto', right = 100, bottom = 100,
      top = 60, right = 'auto', left = 50, bottom = 'auto',
      
      # panel contents
      
      # 1. data selection (div and id is for shinyjs later application)
      div(
        id = 'dataSel',
        
        fluidRow(
          column(
            4,
            selectInput(
              ns('ifn'), 'Versió de les dades', ifns,
              selected = 'ifn2'
            )
          ),
          column(
            4,
            selectInput(
              ns('admin_div'), 'Divisions administratatives', admin_divs,
              selected = ''
            )
          ),
          column(
            4,
            selectInput(
              ns('espai_tipus'), "Tipus d'espai", espai_tipus,
              selected = 'proteccio'
            )
          )
        )
      ),
      
      # 2. data filtering (div and id is for shinyjs later application)
      #   (this inputs are created empty and filled later on in the server based
      #   on the section 1. inputs)
      div(
        id = 'dataFil',
        
        # horizontal rule to separate
        hr(),
        
        fluidRow(
          column(
            6,
            selectInput(
              ns('admin_div_fil'), 'Filtra per division administrative',
              choices = c('Tota Catalunya' = ''),
              selected = '', multiple = TRUE, width = '100%'
            )
          ),
          column(
            6,
            selectInput(
              ns('espai_tipus_fil'), "Filtra per tipus d'espai",
              choices = c('Totes' = ''),
              selected = '', multiple = TRUE, width = '100%'
            )
          )
        ),
        fluidRow(
          actionButton(
            ns('apply_filters'), 'Aplicar filtres', width = '35%' 
          )
        )
      ),
      
      # 3. data aggregation level (div and id is for shinyjs later application)
      div(
        id = 'dataAgg',
        
        # horizontal rule to separate
        hr(),
        
        fluidRow(
          column(
            9,
            selectInput(
              ns('agg_level'), "Nivell d'agregació", agg_levels,
              selected = 'parcela', width = '100%'
            )
          ),
          column(
            3,
            checkboxInput(
              ns('diam_class'), '¿Desglossar per classes diametriques?',
              value = FALSE
            )
          )
        )
      )
      
    ) # absolute panel end
    
  ) # end of tagList
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @export
#' 
#' @rdname mod_dataUI
mod_data <- function(
  input, output, session
) {
  
  # observers to update the dataFil inputs
  observe({
    # create the input choices based on the administrative division input
    admin_div_sel <- input$admin_div
    if (is.null(admin_div_sel) | admin_div_sel == '') {
      
      # admin_div_fil_choices <- list(
      #   catalunya = ''
      # )
      
      # if catalunya is selected, the filter has no sense (there is nothing
      # to filter by), so we disable the input with shinyjs, but before that
      # we update the input to show the original title (if not, the title is
      # stuck with the last admin div selected)
      updateSelectInput(
        session, 'admin_div_fil', 'Filtra per division administrative',
        choices = c('Tota Catalunya' = ''),
        selected = ''
      )
      
      disable('admin_div_fil')
      
    } else {
      admin_div_fil_choices <- noms_divs
      updateSelectInput(
        session, 'admin_div_fil', label = paste0('Filtra per ', admin_div_sel),
        choices = admin_div_fil_choices,
        selected = admin_div_fil_choices[[admin_div_sel]][1]
      )
    }
  })
  
  observe({
    # get the protection level and create the choices based on the dic
    espai_tipus_sel <- input$espai_tipus
    espai_tipus_fil_choices <- proteccion_dictionary[[espai_tipus_sel]]
    
    updateSelectInput(
      session, 'espai_tipus_fil', label = paste0('Filtra per ', espai_tipus_sel),
      choices = espai_tipus_fil_choices,
      selected = espai_tipus_fil_choices[1]
    )
  })
  
  # data reactives to create (sig, clima and core). The sig data is the key
  # as it can be filtered by admin divs and espais. Also, is really costy, so
  # it must be only recalculated when the user selection is completly done,
  # so we have to add a button to signal the filtering step. Even more, we need
  # to create an empty data frame in the case of filtering returns no data.
  data_sig <- eventReactive(
    ignoreNULL = FALSE, ignoreInit = FALSE,
    eventExpr = {
      
      # we need to update the data when ifn is changed or when filterings are
      # applied, so we look up for two inputs:
      input$apply_filters
      input$ifn
      
    },
    valueExpr = {
      
      # stuff needed
      sig_name <- paste0('parcela', input$ifn, '_sig_etrs89')
      
      empty <- data_frame(
        # vars for the map
        idparcela = NA, latitude = NA_real_, longitude = NA_real_,
        # vars for the conditional info panel
        provincia = NA, vegueria = NA, comarca = NA, municipi = NA
      )
      
      data_sig_init <- tbl(oracle_ifn, sig_name)
      
      # if apply_filters button is not pressed, then return the initial data
      if (input$apply_filters == 0) {
        return(data_sig_init)
      } else {
        
        # when button is pressed, then all the logic start working
        if (is.null(input$admin_div_fil)) {
          filter_expr_admin <- quo(TRUE)
        } else {
          filter_expr_admin <- quo(!!sym(input$admin_div) %in% !!input$admin_div_fil)
        }
        
        if (is.null(input$espai_tipus_fil)) {
          filter_expr_espai <- quo(TRUE)
        } else {
          filter_expr_espai <- quo(!!sym(input$espai_tipus) %in% !!input$espai_tipus_fil)
        }
        
        tbl(oracle_ifn, sig_name) %>%
          filter(!!! filter_expr_admin, !!! filter_expr_espai)
        
      }
      
      
    }
  )
  
  
  # data_sig <- reactive({
  #   
  #   sig_name <- paste0('parcela', input$ifn, '_sig_etrs89')
  #   
  #   # filters based on the dataFil inputs
  #   # filter_exprs <- quos(
  #   #   !!sym(input$admin_div) %in% !!input$admin_div_fil,
  #   #   !!sym(input$espai_tipus) %in% !!input$espai_tipus_fil
  #   # )
  #   # if the admin_div is '' then there is no filter by admin_div
  #   if (is.null(input$admin_div_fil)) {
  #     filter_expr_admin <- quo(TRUE)
  #   } else {
  #     filter_expr_admin <- quo(!!sym(input$admin_div) %in% !!input$admin_div_fil)
  #   }
  #   
  #   if (is.null(input$espai_tipus_fil)) {
  #     filter_expr_espai <- quo(TRUE)
  #   } else {
  #     filter_expr_espai <- quo(!!sym(input$espai_tipus) %in% !!input$espai_tipus_fil)
  #   }
  #   
  #   tbl(oracle_ifn, sig_name) %>%
  #     filter(!!! filter_expr_admin, !!! filter_expr_espai)
  #   
  # })
  
  data_clima <- reactive({
    
    clima_name <- paste0('parcela', input$ifn, '_clima')
    # idparcelas to filter based on data_sig()
    idparcelas <- data_sig() %>% pull(idparcela)
    
    tbl(oracle_ifn, clima_name) %>%
      filter(idparcela %in% idparcelas)
    
  })
  
  data_core <- reactive({
    
    ifn <- input$ifn
    agg <- input$agg_level
    cd <- if (isTRUE(input$diam_class)) {'cd'} else {''}
    idparcelas <- data_sig() %>% pull(idparcela)
    
    # real time calculations
    if (stringr::str_detect(agg, '_rt')) {
      return() #TODO
    } else {
      
      # parcela, no aggregation level
      if (agg == 'parcela') {
        if (cd == '') {
          core_name <- paste0('r_', ifn)
        } else {
          core_name <- paste0('r_', cd, '_', ifn)
        }
      } else {
        core_name <- paste0('r_', agg, cd, '_', ifn)
      }
    }
    
    tbl(oracle_ifn, core_name) %>%
      filter(idparcela %in% idparcelas)
    
  })
  
  # reactive values to return for use in other modules
  data_reactives <- reactiveValues()
  
  observe({
    # inputs
    data_reactives$ifn <- input$ifn
    data_reactives$admin_div <- input$admin_div
    data_reactives$espai_tipus <- input$espai_tipus
    data_reactives$admin_div_fil <- input$admin_div_fil
    data_reactives$espai_tipus_fil <- input$espai_tipus_fil
    data_reactives$agg_level <- input$agg_level
    data_reactives$diam_class <- input$diam_class
    
    # data
    data_reactives$data_sig <- data_sig
    data_reactives$data_clima <- data_clima
    data_reactives$data_core <- data_core
    
  })
  
  return(data_reactives)
}
