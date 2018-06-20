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
  
  # inputs choices
  ifns <- c(
    'IFN 2' = 'ifn2',
    'IFN 3' = 'ifn3',
    'IFN 4' = 'ifn4',
    'IFN 3 respecte a IFN 2' = 'ifn3_ifn2',
    'IFN 4 respecte a IFN 3' = 'ifn3_ifn4'
  )
  
  admin_divs <- c(
    Catalunya = 'catalunya', Provincies = 'provincia', Vegueries = 'vegueria',
    Comarques = 'comarca', Municipis = 'municipi'
  )
  
  noms_divs = list(
    comarca = c('Totes', sort(as.character(polygons_comarques@data$NOM_COMAR))),
    municipi = c('Tots', sort(as.character(polygons_municipis@data$NOM_MUNI))),
    vegueria = c('Totes', sort(as.character(polygons_vegueries@data$NOMVEGUE))),
    provincia = c('Totes', sort(as.character(polygons_provincies@data$NOM_PROV)))
  )
  
  espai_tipus <- c(
    'Nivell de protecció' = 'proteccio',
    "Espai d'interès Nacional" = 'nomein',
    "Espai de protecció especial" = 'enpes',
    "Xarxa Natura 2000" = 'nomxarxa2000'
  )
  
  agg_levels <- list(
    
    "Parcel·les" = list(
      'Parcel·la' = 'parcela',
      'Parcel·la desglossat per Espècie' = 'especie',
      'Parcel·la desglossat per Espècie simplificat' = 'espsimple',
      'Parcel·la desglossat per Gènere' = 'genere',
      'Parcel·la desglossat per Conífera/Caducifoli/Esclerofil·le' = 'cadesclcon',
      'Parcel·la desglossat per Conífera/Planifoli' = 'plancon'
    ),
    
    "Grups funcionals" = list(
      'Espècie dominant' = 'especie_rt',
      'Espècie simplificat dominant' = 'espsimple_rt',
      'Gènere dominant' = 'genere_rt',
      'Conífera/Caducifoli/Esclerofil·le dominant' = 'cadesclcon_rt',
      'Conífera/Planifoli dominant' = 'plancon_rt'
    ),
    'Administratiu' = list("Divisions seleccionats" = 'territori_rt')
  )
  
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
              selected = 'catalunya'
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
              ns('admin_div_fil'), '', 'Totes',
              selected = 'Totes', multiple = TRUE, width = '100%'
            )
          ),
          column(
            6,
            selectInput(
              ns('espai_tipus_fil'), '', 'Totes',
              selected = 'Totes', multiple = TRUE, width = '100%'
            )
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
    if (is.null(admin_div_sel)) {
      admin_div_fil_choices <- list(
        catalunya = '', provincia = '', vegueria = '',
        comarca = '', municipi = ''
      )
    } else {
      admin_div_fil_choices <- noms_divs
    }
    
    updateSelectInput(
      session, 'admin_div_fil', label = paste0('Filtra per ', admin_div_sel),
      choices = admin_div_fil_choices,
      selected = admin_div_fil_choices[[admin_div_sel]][1]
    )
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
  
  # data reactives to create (sig, clima and core)
  data_sig <- reactive({
    
    sig_name <- paste0('parcela', input$ifn, '_sig_etrs89')
    # filters based on the dataFil inputs
    filter_exprs <- quos(
      !!admin_div %in% !!input$admin_div_fil,
      !!espai_tipus %in% !!input$espai_tipus_fil
    )
    
    tbl(oracle_ifn, sig_name) %>%
      dplyr::filter(!!! filter_exprs)
    
  })
  
  data_clima <- reactive({
    
    clima_name <- paste0('parcela', input$ifn, '_clima')
    # idparcelas to filter based on data_sig()
    idparcelas <- data_sig() %>% pull(idespecie)
    
    tbl(oracle_ifn, clima_name) %>%
      filter(idparcela %in% idparcelas)
    
  })
  
  data_core <- reactive({
    
    ifn <- input$ifn
    agg <- input$agg_level
    cd <- if (isTRUE(input$diam_class)) {'cd'} else {''}
    idparcelas <- data_sig() %>% pull(idespecie)
    
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
      filter(idespecie %in% idespecies)
    
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
