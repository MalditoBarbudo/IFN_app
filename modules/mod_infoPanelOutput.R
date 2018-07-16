#' @title mod_infoPanelOutput and mod_infoPanel
#' 
#' @description A shiny module to generate the data tables
#' 
#' @param id shiny id
#' 
#' @export
mod_infoPanelOutput <- function(id) {
  
  # ns
  ns <- NS(id)
  
  # UI outputs
  tagList(
    absolutePanel(
      # panel settings
      id = 'infoPanel', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 640, height = 'auto',
      top = 'auto', left = 'auto', bottom = 0, right = 15,
      
      # panel contents
      tabsetPanel(
        id = 'infoPanel_tabs', type = 'pills',
        
        tabPanel(
          'Info',
          uiOutput(ns('shape_click_info')),
          'Aquí va la info de la parcela o del polígono clickado'
        ),
        
        tabPanel(
          'Visualització',
          plotOutput(ns('shape_click_plot'), width = 600, height = 350) %>%
            withSpinner(
              type = 4, color = '#D2527F'
            )
        )
      )
    )
  )
}

#' mod_infoPanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_map reactive with the map events from the map module
#' 
#' @export
#' 
#' @rdname mod_infoPanelOutput
mod_infoPanel <- function(
  input, output, session,
  mod_data, mod_map
) {
  
  # shape data, we need an eventReactive
  data_shape <- eventReactive(
    ignoreInit = FALSE,
    
    eventExpr = mod_map$map_shape_click,
    valueExpr = {
      
      # inputs
      click <- mod_map$map_shape_click
      
      # filter expression
      filter_expr <- quo(!!sym(click$group) == click$id)
      
      # scenario
      input_scenario <- reactive({
        get_scenario(mod_data$viz_shape, mod_data$agg_level)
      })
      
      # aggregation level
      agg_real <- mod_data$agg_level
      
      
      ## debug
      ## remove
      # browser()
      
      # data. We dont use data_core() or data_viz() because we want to override
      # the agg input as we dont want to work here with any _rt or territori_
      # variants as they return unusable data for this plotting panel. So we
      # use the data_generator function, overriding agg with the agg_real value
      # This way we don't need to even worry about if click is in plot or in
      # polygon and things like that.
      idparcelas <- mod_data$data_sig() %>%
        filter(!!! filter_expr) %>%
        select(idparcela) %>%
        collect() %>%
        pull(idparcela)
      
      res <- data_generator(
        sql_db = oracle_ifn,
        ifn = mod_data$ifn,
        viz_shape = 'parcela',
        agg_level = agg_real,
        admin_div = NULL,
        diam_class = mod_data$diam_class,
        data_sig = mod_data$data_sig(),
        .funs = NULL
      ) %>%
        filter(idparcela %in% idparcelas)
    }
  )
  
  # plotting
  output$shape_click_plot <- renderPlot({
    
    click <- mod_map$map_shape_click
    agg_real <- mod_data$agg_level %>%
      stringr::str_remove('_rt') %>%
      stringr::str_remove('territori_')
    
    group <- if (click$group == 'idparcela') {
      click$group
    } else {'other'}
    
    ## debug
    ## remove this
    # browser()
    
    x_density_plot <- quo(!!sym(plots_dictionary[[group]][['densitat']][[agg_real]][['x']]))
    y_density_plot <- quo(!!sym(plots_dictionary[[group]][['densitat']][[agg_real]][['y']]))
    fill_density_plot <- quo(!!sym(plots_dictionary[[group]][['densitat']][[agg_real]][['fill']]))
    fill_col_density_plot <- plots_dictionary[[group]][['densitat']][[agg_real]][['fill_col']]
    y_lims_density_plot <- plots_dictionary[[group]][['densitat']][[agg_real]][['y_lims']]
    title_density_plot <- plots_dictionary[[group]][['densitat']][[agg_real]][['title']]
    subtitle_density_plot <- plots_dictionary[[group]][['densitat']][[agg_real]][['subtitle']]
    plot_list_density <- plots_dictionary[[group]][['densitat']][[agg_real]][['plot_list']]
    
    x_ab_plot <- quo(!!sym(plots_dictionary[[group]][['ab']][[agg_real]][['x']]))
    y_ab_plot <- quo(!!sym(plots_dictionary[[group]][['ab']][[agg_real]][['y']]))
    fill_ab_plot <- quo(!!sym(plots_dictionary[[group]][['ab']][[agg_real]][['fill']]))
    fill_col_ab_plot <- plots_dictionary[[group]][['ab']][[agg_real]][['fill_col']]
    y_lims_ab_plot <- plots_dictionary[[group]][['ab']][[agg_real]][['y_lims']]
    title_ab_plot <- plots_dictionary[[group]][['ab']][[agg_real]][['title']]
    subtitle_ab_plot <- plots_dictionary[[group]][['ab']][[agg_real]][['subtitle']]
    plot_list_ab <- plots_dictionary[[group]][['ab']][[agg_real]][['plot_list']]
    
    plot_density <- data_shape() %>%
      ggplot(aes(!!x_density_plot, !!y_density_plot, fill = !!fill_density_plot)) +
      plot_list_density +
      labs(
        title = title_density_plot, subtitle = subtitle_density_plot
      ) +
      scale_y_continuous(
        limits = y_lims_density_plot, expand = expand_scale(0,0)
      ) +
      theme_infoPanel
    
    plot_ab <- data_shape() %>%
      ggplot(aes(!!x_ab_plot, !!y_ab_plot, fill = !!fill_ab_plot)) +
      plot_list_ab +
      labs(
        title = title_ab_plot, subtitle = subtitle_ab_plot
      ) +
      scale_y_continuous(
        limits = y_lims_ab_plot, expand = expand_scale(0,0)
      ) +
      theme_infoPanel
    
    # return both plots as one thanks to patchwork package
    plot_density + plot_ab
    
  })
  
  # info tab ui
  output$shape_click_info <- renderUI({
    
    click <- mod_map$map_shape_click
    
    #debug
    #remove
    # browser()
    # info for plots
    if (click$group == 'idparcela') {
      
      info_sig <- mod_data$data_sig() %>%
        filter(idparcela == click$id) %>%
        select(idparcela, altitud, pendentpercentatge, proteccio) %>%
        collect()
      
      tagList(
        h4(paste0('Parcela: #', info_sig[['idparcela']][1])),
        strong(sprintf('altitud: %1.f %%', info_sig[['altitud']][1])),
        br(),
        strong(sprintf('pendent: %1.f %%', info_sig[['pendentpercentatge']][1])),
        br(),
        strong(sprintf('nivell de protecció: %s', info_sig[['proteccio']][1]))
      )
      
      # info for polygons
    } else {
      
      info_sig <- mod_data$data_sig() %>%
        filter(!!sym(mod_data$admin_div) == click$id) %>%
        select(idparcela, altitud, pendentpercentatge, proteccio) %>%
        collect()
      
      tagList(
        h4(paste0(
          length(unique(info_sig[['idparcela']])), ' parcel·les en ', click$id
        ))
      )
    }
    
  })
  
  infoPanel_reactives <- reactiveValues()

  observe({
    infoPanel_reactives$data_shape <- data_shape
  })

  return(infoPanel_reactives)
}
