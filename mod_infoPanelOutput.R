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
          'Mapa click',
          uiOutput(ns('shape_click_info')),
          br(),
          plotOutput(ns('shape_click_plot'), width = 540),
          br(),
          textOutput(ns('infoPanel_debug'))
        ),
        
        tabPanel(
          'Informació general',
          'Que va aqui????'
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
    eventExpr = mod_map$map_shape_click,
    valueExpr = {
      
      # inputs
      click <- mod_map$map_shape_click
      
      # filter expression
      filter_expr <- quo(!! click$group == click$id)
      
      # data
      mod_data$data_sig() %>%
        filter(!!! filter_expr) %>%
        inner_join(mod_data$data_clima(), by = 'idparcela') %>%
        inner_join(mod_data$data_core(), by = 'idparcela') %>%
        collect()
    }
  )
  
  # plotting
  output$shape_click_plot <- renderPlot({
    
    data_plot <- data_shape()
    click <- mod_map$map_shape_click
    cd <- mod_data$diam_class
    
    # if we click on parceles
    if (click$group == 'idparcela') {
      
      # diameter classes??
      if (cd) {
        plot_densitat <- ggplot(aes(
          x = forcats::as_factor(idcd), y = densitat
        )) +
          geom_col(fill = '#440154FF') +
          labs(
            title = 'Densitat per classe diamètrica'
          ) +
          theme_void() + theme(legend.position = 'none')
        
        plot_ab <- ggplot(aes(
          x = forcats::as_factor(idcd), y = ab
        )) +
          geom_col(fill = '#440154FF') +
          labs(
            title = 'Àrea basal per classe diamètrica'
          ) +
          theme_void() + theme(legend.position = 'none')
      } else {
        # if not diameter classes
        # plot_densitat <- ggplot(aes(
        #   x = 
        # ))
      }
    }
    
  })
  
}
