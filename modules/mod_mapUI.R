#' @title mod_mapUI and mod_map
#' 
#' @description A shiny module to generate the data tables
#' 
#' @param id shiny id
#' 
#' @export
mod_mapUI <- function(id) {
  
  # ns
  ns <- NS(id)
  
  leafletOutput(ns('map'), width = '100%', height = '100%')
  
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#' 
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_viz reactive with the inputs from viz module
#' 
#' @export
#' 
#' @rdname mod_mapUI
mod_map <- function(
  input, output, session,
  mod_data, mod_viz
) {
  
  # noms division
  nom_provincies <- as.character(polygons_provincies@data$NOM_PROV)
  nom_vegueries <- as.character(polygons_vegueries@data$NOMVEGUE)
  nom_comarques <- as.character(polygons_comarques@data$NOM_COMAR)
  nom_municipis <- as.character(polygons_municipis@data$NOM_MUNI)
  # noms proteccions
  nom_enpe <- as.character(polygons_enpe@data$nom)
  nom_pein <- as.character(polygons_pein@data$nom)
  nom_xn2000 <- as.character(polygons_xn2000@data$nom_n2)
  
  # basic map
  # here we only set the view, zoom and the panes for managing the zIndex)
  output$map <- renderLeaflet({
    
    leaflet() %>%
      setView(0.8, 41.67, zoom = 8) %>%
      addMapPane('admin_divs', zIndex = 410) %>%
      addMapPane('proteccions', zIndex = 405) %>%
      addMapPane('parceles', zIndex = 420)
  })
  
  # observer for admin divs polygons. We use this instead of add polygons
  # directly in the map and control them with the default addLayersControl
  # because some ids are identical between polygons layers (i.e. Barcelona in
  # provincies and comarques) which causes some polygons to dissapear after
  # drawing. Also, in this way the app load is faster, but the polygon layer is
  # slower, though. So we control the polygons drawing with a classic
  # input-observer pair, as we do with the parceles circles.
  observeEvent(
    eventExpr = mod_data$admin_div,
    handlerExpr = {
      admin_div <- mod_data$admin_div
      
      if (admin_div == '') {
        leafletProxy('map') %>%
          clearGroup('vegueria') %>%
          clearGroup('comarca') %>%
          clearGroup('municipi') %>%
          clearGroup('provincia')
      } else {
        leafletProxy('map') %>%
          clearGroup('vegueria') %>%
          clearGroup('comarca') %>%
          clearGroup('municipi') %>%
          clearGroup('provincia') %>%
          addPolygons(
            data = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['polygon']])),
            group = polygons_dictionary[[admin_div]][['group']],
            label = polygons_dictionary[[admin_div]][['label']],
            layerId = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['layerId']])),
            weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fill = TRUE,
            color = '#6C7A89FF', fillColor = "#CF000F00",
            highlightOptions = highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE,
              fill = TRUE, fillColor = "#CF000F00"
            ),
            options = pathOptions(
              pane = 'admin_divs'
            )
          )
      }
    }
    
  )
  # observe({
  #   
  #   admin_div <- mod_data$admin_div
  #   
  #   if (admin_div == '') {
  #     leafletProxy('map') %>%
  #       clearGroup('vegueria') %>%
  #       clearGroup('comarca') %>%
  #       clearGroup('municipi') %>%
  #       clearGroup('provincia')
  #   } else {
  #     leafletProxy('map') %>%
  #       clearGroup('vegueria') %>%
  #       clearGroup('comarca') %>%
  #       clearGroup('municipi') %>%
  #       clearGroup('provincia') %>%
  #       addPolygons(
  #         data = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['polygon']])),
  #         group = polygons_dictionary[[admin_div]][['group']],
  #         label = polygons_dictionary[[admin_div]][['label']],
  #         layerId = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['layerId']])),
  #         weight = 1, smoothFactor = 0.5,
  #         opacity = 1.0, fill = TRUE,
  #         color = '#6C7A89FF', fillColor = "#CF000F00",
  #         highlightOptions = highlightOptions(
  #           color = "#CF000F", weight = 2,
  #           bringToFront = FALSE,
  #           fill = TRUE, fillColor = "#CF000F00"
  #         ),
  #         options = pathOptions(
  #           pane = 'admin_divs'
  #         )
  #       )
  #   }
  # })
  
  # observer for proteccions polygons, same as above
  observeEvent(
    eventExpr = mod_data$espai_tipus,
    handlerExpr = {
      
      espai_tipus <- mod_data$espai_tipus
      if (is.null(espai_tipus)) {
        return()
      }
      
      if (espai_tipus == 'proteccio') {
        leafletProxy('map') %>%
          clearGroup('enpes') %>%
          clearGroup('nomxarxa2000') %>%
          clearGroup('nomein')
      } else {
        leafletProxy('map') %>%
          clearGroup('enpes') %>%
          clearGroup('nomxarxa2000') %>%
          clearGroup('nomein') %>%
          addPolygons(
            data = rlang::eval_tidy(sym(polygons_dictionary[[espai_tipus]][['polygon']])),
            group = polygons_dictionary[[espai_tipus]][['group']],
            label = polygons_dictionary[[espai_tipus]][['label']],
            layerId = rlang::eval_tidy(sym(polygons_dictionary[[espai_tipus]][['layerId']])),
            weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fill = TRUE,
            color = '#6C7A89FF', fillColor = "#CF000F00",
            highlightOptions = highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE,
              fill = TRUE, fillColor = "#CF000F00"
            ),
            options = pathOptions(
              pane = 'proteccions'
            )
          )
      }
    }
  )
  # observe({
  #   
  #   espai_tipus <- mod_data$espai_tipus
  #   if (is.null(espai_tipus)) {
  #     return()
  #   }
  #   
  #   if (espai_tipus == 'proteccio') {
  #     leafletProxy('map') %>%
  #       clearGroup('enpes') %>%
  #       clearGroup('nomxarxa2000') %>%
  #       clearGroup('nomein')
  #   } else {
  #     leafletProxy('map') %>%
  #       clearGroup('enpes') %>%
  #       clearGroup('nomxarxa2000') %>%
  #       clearGroup('nomein') %>%
  #       addPolygons(
  #         data = rlang::eval_tidy(sym(polygons_dictionary[[espai_tipus]][['polygon']])),
  #         group = polygons_dictionary[[espai_tipus]][['group']],
  #         label = polygons_dictionary[[espai_tipus]][['label']],
  #         layerId = rlang::eval_tidy(sym(polygons_dictionary[[espai_tipus]][['layerId']])),
  #         weight = 1, smoothFactor = 0.5,
  #         opacity = 1.0, fill = TRUE,
  #         color = '#6C7A89FF', fillColor = "#CF000F00",
  #         highlightOptions = highlightOptions(
  #           color = "#CF000F", weight = 2,
  #           bringToFront = FALSE,
  #           fill = TRUE, fillColor = "#CF000F00"
  #         ),
  #         options = pathOptions(
  #           pane = 'proteccions'
  #         )
  #       )
  #   }
  # })
  
  # observer for visual candy in the map (color and size of parceles or color
  # of the polygons in the administratiu aggregation levels)
  observeEvent(
    eventExpr = {
      if (all(
        is.null(mod_viz$color) || mod_viz$color == '',
        is.null(mod_viz$mida) || mod_viz$mida == '',
        is.null(mod_viz$inverse_pal) || mod_viz$inverse_pal == '',
        # is.null(mod_data$agg_level) || mod_data$agg_level == ''
        is.null(mod_data$data_sig())
      )) {
        return(NULL)
      } else {
        TRUE
      }
    },
    handlerExpr = {
      
      # stuff needed
      color_var <- mod_viz$color
      mida_var <- mod_viz$mida
      inverse_pal <- mod_viz$inverse_pal
      
      # mod_data stuff
      agg <- mod_data$agg_level
      
      # parceles, tipus and derivats (points!!!)
      if (agg %in% c(
        'parcela', 'especie', 'espsimple', 'genere', 'cadesclcon', 'plancon',
        'especie_rt', 'espsimple_rt', 'genere_rt', 'cadesclcon_rt', 'plancon_rt'
      )) {
        
        vars_sel <- quos(
          !!sym(color_var), !!sym(mida_var),
          !!sym('latitude'), !!sym('longitude'), !!sym('idparcela')
        )
        
        # check for any empty (color or mida) and remove it from the quosures
        vars_sel <- vars_sel[!vapply(vars_sel, rlang::quo_is_missing, logical(1))]
        
        data_parceles <- mod_data$data_viz() %>%
          inner_join(mod_data$data_sig(), by = 'idparcela') %>% 
          inner_join(mod_data$data_clima(), by = 'idparcela') %>% 
          dplyr::select(!!! vars_sel) %>% 
          collect()
        
        # color palette
        if (is.null(color_var) || color_var == '') {
          color_vector <- rep('parcel·la', nrow(data_parceles))
          pal <- colorFactor('viridis', color_vector)
        } else {
          
          # We must take into account if the variable is categorical or
          # numerical
          color_vector <- data_parceles[[color_var]]
          if (is.numeric(color_vector)) {
            pal <- colorBin('viridis', color_vector, 9, reverse = inverse_pal)
          } else {
            pal <- colorFactor('viridis', color_vector, reverse = inverse_pal)
          }
        }
        
        # size vector
        if (is.null(mida_var) || mida_var == '') {
          mida_vector <- rep(750, nrow(data_parceles))
        } else {
          # We must take into account if the variable is categorical or
          # numerical
          mida_var_values <- data_parceles[[mida_var]]
          if (is.numeric(mida_var_values)) {
            mida_vector <- mida_var_values / max(mida_var_values, na.rm = TRUE) * 3000
          } else {
            mida_vector <- rep(750, nrow(data_parceles))
          }
        }
        
        # update map
        leafletProxy('map', data = data_parceles) %>%
          clearGroup('idparcela') %>%
          addCircles(
            group = 'idparcela', lng = ~longitude, lat = ~latitude,
            label = ~idparcela, layerId = ~idparcela,
            stroke = FALSE, fillOpacity = 0.4, fillColor = pal(color_vector),
            radius = mida_vector,
            options = pathOptions(pane = 'parceles')
          ) %>%
          addLegend(
            position = 'topright', pal = pal, values = color_vector,
            title = color_var, layerId = 'color_legend'
          )
      } else {
        
        # administratiu (polygons!!!)
        vars_sel <- quos(
          !!sym(color_var), !!sym(mida_var),
          !!sym('latitude'), !!sym('longitude'), !!sym('idparcela')
        )
        
        # check for any empty (color or mida) and remove it from the quosures
        vars_sel <- vars_sel[!vapply(vars_sel, rlang::quo_is_missing, logical(1))]
        
        grup_fun_val <- agg %>%
          stringr::str_remove('_rt') %>%
          stringr::str_remove('territori_') %>%
          paste0('id',.)
        
        data_parceles <- mod_data$data_viz() %>%
          filter(!!sym(grup_fun_val) == mida_var) %>%
          collect()
        
        admin_div <- mod_data$admin_div
        
        # color palette
        if (is.null(color_var) || color_var == '') {
          color_vector <- rep('parcel·la', nrow(data_parceles))
          pal <- colorFactor('viridis', color_vector)
        } else {
          
          # We must take into account if the variable is categorical or
          # numerical
          color_vector <- data_parceles[[color_var]]
          if (is.numeric(color_vector)) {
            pal <- colorBin('viridis', color_vector, 9, reverse = inverse_pal)
          } else {
            pal <- colorFactor('viridis', color_vector, reverse = inverse_pal)
          }
        }
        
        if (admin_div == '') {
          leafletProxy('map') %>%
            clearGroup('vegueria') %>%
            clearGroup('comarca') %>%
            clearGroup('municipi') %>%
            clearGroup('provincia')
        } else {
          leafletProxy('map') %>%
            clearGroup('vegueria') %>%
            clearGroup('comarca') %>%
            clearGroup('municipi') %>%
            clearGroup('provincia') %>%
            clearGroup('idparcela') %>%
            addPolygons(
              data = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['polygon']])),
              group = polygons_dictionary[[admin_div]][['group']],
              label = polygons_dictionary[[admin_div]][['label']],
              layerId = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['layerId']])),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, color = '#6C7A89FF',
              fill = TRUE, fillColor = pal(color_vector),
              highlightOptions = highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE,
                fill = FALSE
              ),
              options = pathOptions(
                pane = 'admin_divs'
              )
            ) %>%
            addLegend(
              position = 'topright', pal = pal, values = color_vector,
              title = color_var, layerId = 'color_legend'
            )
        }
      }
    }
  )
  
  
  # observe({
  # 
  #   # stuff needed
  #   color_var <- mod_viz$color
  #   mida_var <- mod_viz$mida
  #   inverse_pal <- mod_viz$inverse_pal
  #   
  #   # mod_data stuff
  #   agg <- mod_data$agg_level
  #   
  #   # parceles, tipus and derivats (points!!!)
  #   if (agg %in% c(
  #     'parcela', 'especie', 'espsimple', 'genere', 'cadesclcon', 'plancon',
  #     'especie_rt', 'espsimple_rt', 'genere_rt', 'cadesclcon_rt', 'plancon_rt'
  #   )) {
  #     
  #     vars_sel <- quos(
  #       !!sym(color_var), !!sym(mida_var),
  #       !!sym('latitude'), !!sym('longitude'), !!sym('idparcela')
  #     )
  #     
  #     # check for any empty (color or mida) and remove it from the quosures
  #     vars_sel <- vars_sel[!vapply(vars_sel, rlang::quo_is_missing, logical(1))]
  #     
  #     data_parceles <- mod_data$data_viz() %>%
  #       inner_join(mod_data$data_sig(), by = 'idparcela') %>% 
  #       inner_join(mod_data$data_clima(), by = 'idparcela') %>% 
  #       dplyr::select(!!! vars_sel) %>% 
  #       collect()
  #     
  #     # color palette
  #     if (is.null(color_var) || color_var == '') {
  #       color_vector <- rep('parcel·la', nrow(data_parceles))
  #       pal <- colorFactor('viridis', color_vector)
  #     } else {
  #       
  #       # We must take into account if the variable is categorical or
  #       # numerical
  #       color_vector <- data_parceles[[color_var]]
  #       if (is.numeric(color_vector)) {
  #         pal <- colorBin('viridis', color_vector, 9, reverse = inverse_pal)
  #       } else {
  #         pal <- colorFactor('viridis', color_vector, reverse = inverse_pal)
  #       }
  #     }
  #     
  #     # size vector
  #     if (is.null(mida_var) || mida_var == '') {
  #       mida_vector <- rep(750, nrow(data_parceles))
  #     } else {
  #       # We must take into account if the variable is categorical or
  #       # numerical
  #       mida_var_values <- data_parceles[[mida_var]]
  #       if (is.numeric(mida_var_values)) {
  #         mida_vector <- mida_var_values / max(mida_var_values) * 3000
  #       } else {
  #         mida_vector <- rep(750, nrow(data_parceles))
  #       }
  #     }
  #     
  #     # update map
  #     leafletProxy('map', data = data_parceles) %>%
  #       clearGroup('idparcela') %>%
  #       addCircles(
  #         group = 'idparcela', lng = ~longitude, lat = ~latitude,
  #         label = ~idparcela, layerId = ~idparcela,
  #         stroke = FALSE, fillOpacity = 0.4, fillColor = pal(color_vector),
  #         radius = mida_vector,
  #         options = pathOptions(pane = 'parceles')
  #       ) %>%
  #       addLegend(
  #         position = 'topright', pal = pal, values = color_vector,
  #         title = color_var, layerId = 'color_legend'
  #       )
  #   } else {
  #     
  #     # administratiu (polygons!!!)
  #     vars_sel <- quos(
  #       !!sym(color_var), !!sym(mida_var),
  #       !!sym('latitude'), !!sym('longitude'), !!sym('idparcela')
  #     )
  #     
  #     # check for any empty (color or mida) and remove it from the quosures
  #     vars_sel <- vars_sel[!vapply(vars_sel, rlang::quo_is_missing, logical(1))]
  #     
  #     grup_fun_val <- agg %>%
  #       stringr::str_remove('_rt') %>%
  #       stringr::str_remove('territori_') %>%
  #       paste0('id',.)
  #     
  #     data_parceles <- mod_data$data_viz() %>%
  #       filter(!!sym(grup_fun_val) == mida_var) %>%
  #       collect()
  #     
  #     admin_div <- mod_data$admin_div
  #     
  #     # color palette
  #     if (is.null(color_var) || color_var == '') {
  #       color_vector <- rep('parcel·la', nrow(data_parceles))
  #       pal <- colorFactor('viridis', color_vector)
  #     } else {
  #       
  #       # We must take into account if the variable is categorical or
  #       # numerical
  #       color_vector <- data_parceles[[color_var]]
  #       if (is.numeric(color_vector)) {
  #         pal <- colorBin('viridis', color_vector, 9, reverse = inverse_pal)
  #       } else {
  #         pal <- colorFactor('viridis', color_vector, reverse = inverse_pal)
  #       }
  #     }
  #     
  #     if (admin_div == '') {
  #       leafletProxy('map') %>%
  #         clearGroup('vegueria') %>%
  #         clearGroup('comarca') %>%
  #         clearGroup('municipi') %>%
  #         clearGroup('provincia')
  #     } else {
  #       leafletProxy('map') %>%
  #         clearGroup('vegueria') %>%
  #         clearGroup('comarca') %>%
  #         clearGroup('municipi') %>%
  #         clearGroup('provincia') %>%
  #         clearGroup('idparcela') %>%
  #         addPolygons(
  #           data = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['polygon']])),
  #           group = polygons_dictionary[[admin_div]][['group']],
  #           label = polygons_dictionary[[admin_div]][['label']],
  #           layerId = rlang::eval_tidy(sym(polygons_dictionary[[admin_div]][['layerId']])),
  #           weight = 1, smoothFactor = 0.5,
  #           opacity = 1.0, color = '#6C7A89FF',
  #           fill = TRUE, fillColor = pal(color_vector),
  #           highlightOptions = highlightOptions(
  #             color = "#CF000F", weight = 2,
  #             bringToFront = FALSE,
  #             fill = FALSE
  #           ),
  #           options = pathOptions(
  #             pane = 'admin_divs'
  #           )
  #         ) %>%
  #         addLegend(
  #           position = 'topright', pal = pal, values = color_vector,
  #           title = color_var, layerId = 'color_legend'
  #         )
  #     }
  #   }
  #   
  #   
  #   
  #   # data core alternative if diameter classes is selected. This is due to the
  #   # fact that cd tables are not suitable for plotting the parceles in the map
  #   # as there is several values for each parcele (one per diameter class).
  #   # if (isTRUE(mod_data$diam_class)) {
  #   #   
  #   #   ifn <- mod_data$ifn
  #   #   agg <- mod_data$agg_level
  #   #   idparcelas <- mod_data$data_sig() %>% pull(idparcela)
  #   #   
  #   #   if (agg == 'parcela') {
  #   #     core_name <- paste0('r_', ifn)
  #   #   } else {
  #   #     core_name <- paste0('r_', agg, '_', ifn)
  #   #   }
  #   #   
  #   #   data_parceles <- tbl(oracle_ifn, core_name) %>%
  #   #     filter(idparcela %in% idparcelas) %>%
  #   #     inner_join(mod_data$data_sig(), by = 'idparcela') %>%
  #   #     inner_join(mod_data$data_clima(), by = 'idparcela') %>%
  #   #     dplyr::select(!!! vars_sel) %>%
  #   #     collect()
  #   # } else {
  #   #   data_parceles <- mod_data$data_core() %>%
  #   #     inner_join(mod_data$data_sig(), by = 'idparcela') %>%
  #   #     inner_join(mod_data$data_clima(), by = 'idparcela') %>%
  #   #     dplyr::select(!!! vars_sel) %>%
  #   #     collect()
  #   # }
  #   # 
  #   # # color palette
  #   # if (is.null(color_var) || color_var == '') {
  #   #   color_vector <- rep('parcel·la', nrow(data_parceles))
  #   #   pal <- colorFactor('viridis', color_vector)
  #   # } else {
  #   #   
  #   #   # We must take into account if the variable is categorical or
  #   #   # numerical
  #   #   color_vector <- data_parceles[[color_var]]
  #   #   if (is.numeric(color_vector)) {
  #   #     pal <- colorBin('viridis', color_vector, 9, reverse = inverse_pal)
  #   #   } else {
  #   #     pal <- colorFactor('viridis', color_vector, reverse = inverse_pal)
  #   #   }
  #   # }
  #   # 
  #   # # size vector
  #   # if (is.null(mida_var) || mida_var == '') {
  #   #   mida_vector <- rep(750, nrow(data_parceles))
  #   # } else {
  #   #   # We must take into account if the variable is categorical or
  #   #   # numerical
  #   #   mida_var_values <- data_parceles[[mida_var]]
  #   #   if (is.numeric(mida_var_values)) {
  #   #     mida_vector <- mida_var_values / max(mida_var_values) * 3000
  #   #   } else {
  #   #     mida_vector <- rep(750, nrow(data_parceles))
  #   #   }
  #   # }
  #   # 
  #   # # update map
  #   # leafletProxy('map', data = data_parceles) %>%
  #   #   clearGroup('idparcela') %>%
  #   #   addCircles(
  #   #     group = 'idparcela', lng = ~longitude, lat = ~latitude,
  #   #     label = ~idparcela, layerId = ~idparcela,
  #   #     stroke = FALSE, fillOpacity = 0.4, fillColor = pal(color_vector),
  #   #     radius = mida_vector,
  #   #     options = pathOptions(pane = 'parceles')
  #   #   ) %>%
  #   #   addLegend(
  #   #     position = 'topright', pal = pal, values = color_vector,
  #   #     title = color_var, layerId = 'color_legend'
  #   #   )
  # })
  
  # reactive with the map events
  map_reactives <- reactiveValues()
  
  observe({
    map_reactives$map_shape_click <- input$map_shape_click
    # map_reactives$shape_mouseover <- input$map_shape_mouseover
    # map_reactives$shape_mouseout <- input$map_shape_mouseout
    # map_reactives$map_click <- input$map_click
    # map_reactives$map_bounds <- input$map_bounds
    # map_reactives$map_zoom <- input$map_zoom
    # map_reactives$map_center <- input$map_center
  })
  
  return(map_reactives)
  
}
