#' @title mod_baseTableOutput and mod_baseTable
#' 
#' @description A shiny module to generate the base IFN plots table
#' 
#' @param id shiny id
#' 
#' @export
mod_baseTableOutput <- function(id) {
  
  ns <- NS(id)
  
  # What we need here?
  # We need two columns, one for the table, other one for the settings
  tagList(
    
    fluidRow(
      # data table column
      column(
        8,
        DTOutput(ns('table_dades'))
      ),
      # settings column
      column(
        4,
        wellPanel(
          # filtratge per clima , lo hacemos en el server con una UI
          fluidRow(
            # var sel
            selectInput(
              ns('clima_var_sel'), 'Clima filter',
              choices = clima_vars_dictionary, multiple = TRUE
            ),
            
            uiOutput(
              ns('clim_filters')
            ),
            
            column(3, offset = 9, actionButton(ns('fil_clim_btn'), 'Filtra'))
          ),
          
          # horizontal ruler
          hr(),
          
          # custom plots creator
          fluidRow(
            column(
              4, offset = 4,
              actionButton(
                ns('custom_plot_activator'), 'Gràfiques personalitzades'
              )
            )
          ),
          
          # horizontal ruler
          hr(),
          
          # download config and buttons
          fluidRow(
            # información extra
            column(
              4, offset = 2,
              checkboxGroupInput(
                ns('joined_tbls'), 'Annexar més informació',
                choices = c(
                  "informació SIG" = 'sig_etrs89',
                  "informació climàtica" = 'clima'
                )
              )
            ),
            # buttons
            column(
              4,
              p(strong('Descarregar dades')),
              actionButton(
                ns('csv_dwnl'), 'Guardar CSV', icon = icon('file-alt')
              ),
              actionButton(
                ns('xlsx_dwnl'), 'Guardar xlsx', icon = icon('file-excel')
              )
            )
          ),
          
          textOutput(ns('debug_table'))
        )
      )
    )
  )
}

#' mod_baseTable server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mapControls reactives with the inputs from the mapControls module
#' @param aggregation reactives with the inputs from the aggregation module
#' @param dataReactive reactive data with the map points 
#' @param filterAndSel reactive with the inputs from filterAndSel module
#' 
#' 
#' @export
#' 
#' @rdname mod_baseTable
mod_baseTable <- function(
  input, output, session,
  mapControls, aggregation, dataReactive, filterAndSel, parcelas
) {
  
  # reactives for table names creation
  # general table
  table_name <- reactive({
    
    # inputs
    ifn <- mapControls$ifn
    agg <- aggregation$aggregation_level
    cd <- if (aggregation$diameter_classes) {'cd'} else {''}
    
    # real time calculations TODO!!
    if (stringr::str_detect(agg, '_rt')) {
      return() # TODO
    } else {
      
      # parcela (especial case, as there is no aggregation level)
      if (agg == 'parcela') {
        if (cd == '') {
          table_name <- paste0('r_', ifn)
        } else {
          table_name <- paste0('r_', cd, '_', ifn)
        }
      } else {
        table_name <- paste0('r_', agg, cd, '_', ifn)
      }
    }
    
    return(table_name)
  })
  
  # sig table
  sig_name <- reactive({
    
    # inputs
    ifn <- mapControls$ifn
    # name
    sig_name <- paste0('parcela', ifn, '_sig_etrs89')
    return(sig_name)
  })
  
  # clima table
  clima_name <- reactive({
    
    # inputs
    ifn <- mapControls$ifn
    # name
    clima_name <- paste0('parcela', ifn, '_clima')
  })
  
  # reactives for the data tables callers
  # reactive for general table data
  general_tbl_call <- reactive({
    tbl(oracle_ifn, table_name())
  })
  
  # reactive for sig table
  sig_tbl_call <- reactive({
    tbl(oracle_ifn, sig_name())
  })
  
  # reactive for clima table
  clima_tbl_call <- reactive({
    tbl(oracle_ifn, clima_name())
  })
  
  # DT render
  output$table_dades <- renderDT(
    server = TRUE,
    expr = {
      
      parcelas <- dataReactive() %>%
        pull('idparcela')
      
      general_tbl_call() %>%
        filter(idparcela %in% parcelas) %>%
        collect() %>%
        datatable(
          filter = list(position = 'top', clear = TRUE, plain = FALSE),
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          extensions = c('Buttons', 'Scroller'),
          options = list(
            dom = 'tBi',
            extend = 'collection',
            buttons = c('csv', 'colvis'),
            text = 'Descàrrega',
            autoWidth = TRUE,
            deferRender = TRUE, scrollY = '70vh', scroller = TRUE
          )
        )
    }
  )
  
  # Clima filter inputs as UI
  output$clim_filters <- renderUI({
    
    ns <- session$ns
    clima_data <- clima_tbl_call() %>%
      select_if(is.numeric) %>%
      collect()
    # clima_vars <- names(clima_data)
    
    inputs_list <- reactive({
      lapply(
        input$clima_var_sel, function(var) {
          sliderInput(
            ns(var), label = var,
            min = min(clima_data[[var]]), max = max(clima_data[[var]]),
            value = c(min(clima_data[[var]]), max = max(clima_data[[var]]))
          )
        }
      )
    })
    
    tagList(inputs_list())
    
    # for (var in input$clima_var_sel) {
    #   
    #   sliderInput(
    #     ns(var), label = var,
    #     min = min(clima_data[[var]]), max = min(clima_data[[var]]),
    #     value = c(min(clima_data[[var]]), max = min(clima_data[[var]]))
    #   )
    #   
    # }
    
  })
  
  output$debug_table <- renderPrint({
    input$clima_var_sel
  })
  
  
  # reactiveValues to return
  baseTable_reactives <- reactiveValues()
  
  observe({
    
    baseTable_reactives$table_name <- table_name
    
  })
  
  return(baseTable_reactives)
}
