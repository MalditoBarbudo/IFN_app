#' @title mod_infoPanelUI and mod_infoPanel
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @export
mod_infoPanelUI <- function(id) {
  
  ns <- NS(id)
  
  # A slider-like but with tabs, to show:
  #   a) all points selected and filtered in the map
  #   b) a joined-plot based on the shape map click. For plots:
  #     1. donut plot (idcadescleconif)
  #     2. density with location for some trait (maybe % pies vivos)
  #     3. a radial plot ("rol playing character stats"-like)
  #   c) a joined-plot based on the shape map click. For polygons:
  #     1. Some plot?
  #     2. Some plot?
  #     3. Some plot?
  
  tagList(
    tabsetPanel(
      id = "info_panel_tabs", type = 'pills',
      
      # tab with the shape info
      tabPanel(
        "Click",
        uiOutput(ns('info_shape')),
        br(),
        plotOutput(ns('donut'), height = '185px'),
        br(),
        "Qué más va aqui???",
        br(),
        textOutput(ns('debug'))
      ),
      
      # tab with all the info
      tabPanel(
        "Mapa completo"
      )
    )
  )
}

#' mod_infoPanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data_sig reactive with the data from sigTable module
#' @param data_clima reactive with the data from climaTable module
#' @param data_core reactive with the data from dataTable module
#' @param map_inputs reactive input values from mod_baseMap module
#' @param aggregation reactive values from mod_aggregation module
#' @param data (to be deprecated)
#' 
#' @export
#' 
#' @rdname mod_infoPanel
mod_infoPanel <- function(
  input, output, session,
  map_inputs, aggregation,
  data_sig, data_clima, data_core, data
) {
  
  # sig data
  shape_data <- eventReactive(
    eventExpr = map_inputs$shape_click,
    valueExpr = {
      
      # inputs
      click <- map_inputs$shape_click
      
      # filter expression
      filter_expr <- quo(
        !! click$group == click$id
      )
      
      data_sig() %>%
        # filtrado
        filter(!!! filter_expr) %>%
        # joins
        inner_join(data_clima, by = 'idparcela') %>%
        inner_join(data_core, by = 'idparcela') %>%
        collect()
    }
  )
  
  # plots (using patchwork for all plots in one)
  output$donut <- renderPlot({
    
    plot_data <- shape_data()
    click <- map_inputs$click
    
    # shape == parcela
    if (click$group == 'idparcela') {
      
      # clases diamétricas??
      if (!('planifconifdens' %in% names(plot_data))) {
        
        plot_1 <- ggplot(aes(
          x = forcats::as_factor(idcd), y = densitat
        )) +
          geom_col(fill = '#440154FF') +
          scale_fill_viridis(discrete = TRUE) +
          theme_void() + theme(legend.position = 'none')
        
        plot_2 <- ggplot(aes(
          x = forcats::as_factor(idcd), y = ab
        )) +
          geom_col(fill = '#21908CFF') +
          scale_fill_viridis(discrete = TRUE) +
          theme_void() + theme(legend.position = 'none')
        
      } else {
        
        plot_1 <- ggplot(aes(
          x = planifconifdens, y = percdensplanifconif, fill = percdensplanifconif
        )) + 
          geom_col() +
          coord_polar(theta = 'y') +
          scale_x_continuous(limits = c(.5, 2.5)) +
          # scale_fill_viridis(discrete = TRUE, end = 0.5, name = 'densitat %') +
          scale_fill_manual(name = '', values = c(
            "Conífera" = "#440154FF",
            "Planifoli" = "#21908CFF"
          )) +
          theme_void() +
          theme(legend.position = 'none')
        
        plot_2 <- ggplot(aes(
          x = planifconifdens, y = percabplanifconif, fill = percabplanifconif
        )) + 
          geom_col() +
          coord_polar(theta = 'y') +
          scale_x_continuous(limits = c(.5, 2.5)) +
          # scale_fill_viridis(discrete = TRUE, end = 0.5, name = 'densitat %') +
          scale_fill_manual(name = '', values = c(
            "Conífera" = "#440154FF",
            "Planifoli" = "#21908CFF"
          )) +
          theme_void() +
          theme(legend.position = 'none')
        
      }
      
    }
    
    # shape == polygon
    
  })
  
  
  
}
