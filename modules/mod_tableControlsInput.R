#' @title mod_tableControlsInput and mod_tableControls
#' 
#' @description A shiny module to generate the base IFN plots table
#' 
#' @param id shiny id
#' 
#' @export
mod_tableControlsInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      # First column, aggregation configs
      column(
        2,
        
        # nivel de agregación (parcela, tipo funcional)
        radioButtons(
          ns('agregacion'), "Seleciona el nivell d'agregació",
          choices = c(
            'parcel·les' = 'parceles',
            'tipus funcional' = 'tipu_fun'
          ),
          selected = 'parceles'
        ),
        
        # checkbox to aggregate also by diameter classes
        checkboxInput(
          ns('clas_diam'), "¿Afegir per classes diametriques?",
          value = FALSE
        )
      ),
      
      # Second column, conditional in case of tipu funcional
      div(
        id = ns('togg_tipu_fun'),
        # tipo funcional
        column(
          2,
          radioButtons(
            ns('tipu_fun'), "Selecciona el tipus funcional",
            choices = c(
              'espècie' = 'especie',
              'espècie simplificado' = 'espsimple',
              'gènere' = 'genere',
              'caducifoli/esclerofil/conifer' = 'cadesclcon',
              'planifoli/conifer' = 'plancon'
            ),
            selected = 'especie', inline = TRUE
          )
        ),
        
        # select tipus funcionals
        column(
          3,
          selectInput(
            ns("tipus_selector"), '', 'Totes', selected = 'Totes',
            multiple = TRUE, width = '100%'
          )
        )
      )
    )
  )
}

#' mod_tableControls server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mapControls reactives with the inputs from the map module
#' 
#' 
#' @export
#' 
#' @rdname mod_tableControls
mod_tableControls <- function(
  input, output, session,
  mapControls
) {
  
  
  # observer to toggle the column of tipu_fun
  observe({
    shinyjs::toggleElement(
      "togg_tipu_fun",
      condition = !is.null(input$agregacion) && input$agregacion == "tipu_fun"
    )
  })
  
  # observer to populate the tipus_selector input
  observe({
    ifn <- mapControls$ifn
    tipu_funcional <- input$tipu_fun
    clases_diametricas <- if (input$clas_diam) {'cd'} else {''}
    
    # build the table name
    table_name <- paste0('r_', tipu_funcional, clases_diametricas, '_', ifn)
    
    # get the id column name
    id_var <- c(
      especie_ifn2 = 'idespecieifn2',
      especie_ifn3 = 'idespecie',
      espsimple_ifn2 = 'idespeciesimple',
      espsimple_ifn3 = 'idespeciesimple',
      genere_ifn2 = 'idgenere',
      genere_ifn3 = 'idgenere',
      cadesclcon_ifn2 = 'idcaducesclerconif',
      cadesclcon_ifn3 = 'idcaducesclerconif',
      plancon_ifn2 = 'idplanifconif',
      plancon_ifn3 = 'idplanifconif'
    )[[paste0(tipu_funcional, '_', ifn)]]
    
    id_var <- quo(!! rlang::sym(id_var))
    
    # get the ids
    tipu_fun_ids <- tbl(oracle_ifn, table_name) %>%
      pull(!! id_var) %>%
      unique() %>%
      sort()
    
    # update input
    updateSelectInput(
      session, 'tipus_selector', label = '',
      choices = tipu_fun_ids
    )
    
  })
  
  # observe, collect and return the inputs
  tableControls_inputs <- reactiveValues()
  
  observe({
    
    tableControls_inputs$agregacion <- input$agregacion
    tableControls_inputs$clas_diam <- input$clas_diam
    tableControls_inputs$tipu_fun <- input$tipu_fun
    tableControls_inputs$tipus_selector <- input$tipus_selector
    
  })
  
  return(tableControls_inputs)
}
