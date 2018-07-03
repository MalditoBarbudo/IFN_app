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
          plotOutput(ns('shape_click_plot'), width = 600, height = 350) %>%
            withSpinner(
              type = 4, color = '#D2527F'
            ),
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
#' @param mod_viz reactive with the viz inputs
#' 
#' @export
#' 
#' @rdname mod_infoPanelOutput
mod_infoPanel <- function(
  input, output, session,
  mod_data, mod_map, mod_viz
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
      
      # aggregation level
      agg <- mod_data$agg_level
      
      # agg real time, then data_core() is in charge of everything, we only have
      # to filter by the selected group
      if (stringr::str_detect(agg, '_rt')) {
        
        if (stringr::str_detect(agg, 'territori_')) {
          res <- mod_data$data_core() %>%
            filter(!!! filter_expr) #%>%
            # collect()
        } else {
          # here, is tricky. These are the aggregation levels for tipus
          # funcionals, and per se didn't belong to dots or polygons. So, for
          # the plotting parceles in the infoPanel we need to resort in
          # creating the data de novo
          if (click$group == 'idparcela') {
            # res <- mod_data$data_sig() %>%
            #   filter(!!! filter_expr) %>%
            #   select(idparcela) %>%
            #   inner_join(mod_data$data_viz(), by = 'idparcela') %>%
            #   collect()
            agg_real <- stringr::str_remove(agg, '_rt')
            cd <- if (isTRUE(mod_data$diam_class)) {'cd'} else {''}
            ifn <- mod_data$ifn
            core_name <- paste0('r_', paste0(agg_real, cd, '_'), ifn)
            
            res <- mod_data$data_sig() %>%
              filter(!!! filter_expr) %>%
              select(idparcela) %>%
              inner_join({
                tbl(oracle_ifn, core_name)
              }, by = 'idparcela')
            
          } else {
            # res <- mod_data$data_core() %>%
            #   filter(!!! filter_expr)
            agg_real <- stringr::str_remove(agg, '_rt') %>%
              stringr::str_remove('territori_')
            cd <- if (isTRUE(mod_data$diam_class)) {'cd'} else {''}
            ifn <- mod_data$ifn
            core_name <- paste0('r_', paste0(agg_real, cd, '_'), ifn)
            agg_tipfun_var <- paste0('id', agg_real)
            
            res <- mod_data$data_sig() %>%
              select(idparcela, !!sym(mod_data$admin_div)) %>%
              inner_join(tbl(oracle_ifn, core_name), by = 'idparcela') %>%
              # group_by(!!sym(mod_data$admin_div), !!sym(agg_tipfun_var)) %>%
              # summarise_if(is.numeric, .funs = funs(mean(., na.rm = TRUE))) %>%
              filter(!!! filter_expr) %>%
              collect()
          }
        }
      } else {
        
        # if agg no real time, we need the data_core but also de sig to filter
        # by the selected group
        res <- mod_data$data_sig() %>%
          filter(!!! filter_expr) %>%
          select(idparcela) %>%
          inner_join(mod_data$data_core(), by = 'idparcela') %>%
          collect()
        
      }
      
      return(res)
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
            x = forcats::as_factor(as.character(idcd)), y = densitat
          )) +
            geom_col(fill = '#440154FF') +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica'
            ) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab
          )) +
            geom_col(fill = '#440154FF') +
            labs(
              title = 'Àrea basal',
              subtitle = ' per classe diamètrica'
            ) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = planifconifdens, y = percdensplanifconif,
            fill = planifconifdens
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%)', subtitle = 'del grup funcional dominant'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = planifconifab, y = percabplanifconif, fill = planifconifab
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%)', subtitle = 'del grup funcional dominant'
            ) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            theme_infoPanel
        }
      }
      
      # parcela desglossat per especie
      if (agg_level %in% c('especie', 'especie_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idespecie
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab,
            fill = idespecie
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel + theme(legend.position = c(0.8, 0.8))
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idespecie, y = percdens, fill = idespecie
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%)', subtitle = ' per espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idespecie, y = percab, fill = idespecie
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%)', subtitle = 'per espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcela desglossat per espsimple
      if (agg_level %in% c('espsimple', 'espsimple_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idespeciesimple
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab,
            fill = idespeciesimple
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel + theme(legend.position = c(0.8, 0.8))
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idespeciesimple, y = percdens, fill = idespeciesimple
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%)',
              subtitle = 'per espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idespeciesimple, y = percab, fill = idespeciesimple
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%)',
              subtitle = 'per espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcela desglossat per genere
      if (agg_level %in% c('genere', 'genere_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idgenere
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab, fill = idgenere
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel + theme(legend.position = c(0.8, 0.8))
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idgenere, y = percdens, fill = idgenere
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%)', subtitle = 'per gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idgenere, y = percab, fill = idgenere
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%)', subtitle = 'per gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcele desglossat per cadesclcon
      if (agg_level %in% c('cadesclcon', 'cadesclcon_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idcaducesclerconif
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            ))+
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab, fill = idcaducesclerconif
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel + theme(legend.position = c(0.8, 0.8))
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idcaducesclerconif, y = percdens, fill = idcaducesclerconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%)', subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idcaducesclerconif, y = percab, fill = idcaducesclerconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%)', subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcele desglossat per plancon
      if (agg_level %in% c('plancon', 'plancon_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idplanifconif
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab,
            fill = idplanifconif
          )) +
            geom_col(position = 'dodge') +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel + theme(legend.position = c(0.8, 0.8))
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idplanifconif, y = percdens, fill = idplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Densitat (%)',
              subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idplanifconif, y = percab, fill = idplanifconif
          )) +
            geom_col() +
            labs(
              title = 'Àrea basal (%)',
              subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,100), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
    } else {
      # we click in any polygon (data is filtered by the polygon group, so
      # we only need to plot it in a violin plot or something similar). In this
      # case we will work with absolute values of density and basal area
      
      # parcele level
      if (agg_level == 'parcela') {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat
          )) +
            geom_boxplot(fill = '#440154FF') +
            labs(
              title = 'Densitat', subtitle = 'per classe diamètrica'
            ) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab
          )) +
            geom_boxplot(fill = '#440154FF') +
            labs(
              title = 'Àrea basal', subtitle = 'per classe diamètrica'
            ) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = planifconifdens, y = densitat, fill = planifconifdens
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat',
              subtitle = 'del grup funcional dominant'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = planifconifab, y = ab, fill = planifconifab
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'del grup funcional dominant'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcela desglossat per especie
      if (agg_level %in% c('especie', 'especie_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat, fill = idespecie
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab, fill = idespecie
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idespecie, y = densitat, fill = idespecie
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat', subtitle = 'per espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idespecie, y = ab, fill = idespecie
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'per espècie'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcela desglossat per espsimple
      if (agg_level %in% c('espsimple', 'espsimple_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat, fill = idespeciesimple
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab, fill = idespeciesimple
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idespecie, y = densitat, fill = idespeciesimple
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat', subtitle = 'per espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idespeciesimple, y = ab, fill = idespeciesimple
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'per espècie simplificat'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcela desglossat per genere
      if (agg_level %in% c('genere', 'genere_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat, fill = idgenere
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat', subtitle = 'per classe diamètrica y gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab, fill = idgenere
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'per classe diamètrica y gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idgenere, y = densitat, fill = idgenere
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat', subtitle = 'per gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idgenere, y = ab, fill = idgenere
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'per gènere'
            ) +
            scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcele desglossat per cadesclcon
      if (agg_level %in% c('cadesclcon', 'cadesclcon_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idcaducesclerconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab,
            fill = idcaducesclerconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idcaducesclerconif, y = densitat, fill = idcaducesclerconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat', subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idcaducesclerconif, y = ab, fill = idcaducesclerconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = 'densitat %', values = c(
              "Conífera" = "#440154FF",
              "Caducifoli" = "#3B528BFF",
              "Esclerofil·le" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
      
      # parcele desglossat per plancon
      if (agg_level %in% c('plancon', 'plancon_rt')) {
        
        if (isTRUE(cd)) {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = densitat,
            fill = idplanifconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = forcats::as_factor(as.character(idcd)), y = ab, fill = idplanifconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal',
              subtitle = 'per classe diamètrica y grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
          
        } else {
          
          plot_densitat <- ggplot(data_plot, aes(
            x = idplanifconif, y = densitat, fill = idplanifconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Densitat', subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,3500), expand = expand_scale(0,0)) +
            theme_infoPanel
          
          plot_ab <- ggplot(data_plot, aes(
            x = idplanifconif, y = ab, fill = idplanifconif
          )) +
            geom_boxplot() +
            labs(
              title = 'Àrea basal', subtitle = 'per grup funcional'
            ) +
            scale_fill_manual(name = '', values = c(
              "Conífera" = "#440154FF",
              "Planifoli" = "#21908CFF"
            )) +
            scale_y_continuous(limits = c(0,50), expand = expand_scale(0,0)) +
            theme_infoPanel
        }
      }
    }
    
    plot_densitat + plot_ab
  })
  
  infoPanel_reactives <- reactiveValues()
  
  observe({
    infoPanel_reactives$data_shape <- data_shape
  })
  
  return(infoPanel_reactives)
}
