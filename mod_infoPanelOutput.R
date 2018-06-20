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
      filter_expr <- quo(!!click$group == click$id)
      
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
    agg_level <- mod_data$agg_level
    
    # if we click on parceles
    if (click$group == 'idparcela') {
      
      # now, we navigate for each aggregation level in the parcele level. Not
      # optimal but it will work. Also in each aggregation level we need to
      # check for diameter classes, as the plots are completly different.
      
      # parcele level
      if (agg_level == 'parcela') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = densitat
          )) +
            geom_col(fill = '#440154FF') +
            labs(
              title = 'Densitat per classe diamètrica'
            ) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = ab
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal per classe diamètrica'
            ) +
            theme_void() + theme(legend.position = 'none')
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = planifconifdens, y = percdensplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%) del grup funcional dominant'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = planifconifab, y = percabplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%) del grup funcional dominant'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            theme_void() + theme(legend.position = 'none')
        }
      }
      
      # parcela desglossat per especie
      if (agg_level == 'especie') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = densitat, fill = idespecieifn2
          )) +
            geom_col() +
            labs(
              title = 'Densitat per classe diamètrica y espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = ab, fill = idespecieifn2
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal per classe diamètrica y espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idespecieifn2, y = percdens, fill = idespecieifn2
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%) per espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = idespecieifn2, y = percab, fill = idespecieifn2
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%) per espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
        }
      }
      
      # parcela desglossat per espsimple
      if (agg_level == 'espsimple') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = densitat, fill = idespeciesimple
          )) +
            geom_col() +
            labs(
              title = 'Densitat per classe diamètrica y espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = ab, fill = idespeciesimple
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal per classe diamètrica y espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idespecieifn2, y = percdens, fill = idespeciesimple
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%) per espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = idespeciesimple, y = percab, fill = idespeciesimple
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%) per espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
        }
      }
      
      # parcela desglossat per genere
      if (agg_level == 'genere') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = densitat, fill = idgenere
          )) +
            geom_col() +
            labs(
              title = 'Densitat per classe diamètrica y gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = ab, fill = idgenere
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal per classe diamètrica y gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idgenere, y = percdens, fill = idgenere
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%) per gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = idgenere, y = percab, fill = idgenere
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%) per gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
        }
      }
      
      # parcele desglossat per cadesclcon
      if (agg_level == 'cadesclcon') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = densitat, fill = idcaducesclerconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat per classe diamètrica y grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = ab, fill = idcaducesclerconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal per classe diamètrica y grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idcaducesclerconif, y = percdens, fill = idcaducesclerconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%) per grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = idcaducesclerconif, y = percab, fill = idcaducesclerconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%) per grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
        }
      }
      
      # parcele desglossat per plancon
      if (agg_level == 'plancon') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = densitat, fill = idplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat per classe diamètrica y grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(idcd), y = ab, fill = idplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal per classe diamètrica y grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idplanifconif, y = percdens, fill = idplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%) per grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
          
          plot_ab <- ggplot(data_plot, aes(
            x = idplanifconif, y = percab, fill = idplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%) per grup funcional'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            theme_void() + theme(legend.position = 'none')
        }
      }
    }
    
  })
  
}
