#' @title mod_baseMapOutput and mod_baseMap
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @export
mod_baseMapOutput <- function(id) {
  
  ns <- NS(id)
  
  leafletOutput(ns('baseMap'), width = '100%', height = '100%')
  
}

#' mod_baseMap server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param municipis,comarques,vegueries,provincies polygons objects for each
#'   regional level
#' @param ifn,size,color,inv_pal reactive expresions containing the corresponding
#'   inputs not described in mod_baseMapOutput
#' @param event reactive indicating the shape input click
#' 
#' @export
#' 
#' @rdname mod_baseMapOutput
mod_baseMap <- function(
  input, output, session,
  municipis, comarques, vegueries, provincies,
  ifn, size, color, inv_pal
) {
  
  # data
  data_parcelas <- reactive({
    
    ifn_sel <- ifn()
    
    # tables names, depending on the ifn selected
    clima_name <- paste0('parcela', ifn_sel, '_clima')
    sig_name <- paste0('parcela', ifn_sel, '_sig_etrs89')
    cec_name <- paste0('r_cadesclcon_', ifn_sel)
    
    # table for
    #   1. parcelas
    #   2. colores y tamaños
    #   3. popups
    tbl(oracle_ifn, clima_name) %>%
      inner_join(tbl(oracle_ifn, sig_name), by = 'idparcela') %>%
      inner_join(tbl(oracle_ifn, cec_name), by = 'idparcela') %>%
      # select(idparcela) %>%
      collect()
  })
  
  # actual base map
  output$baseMap <- renderLeaflet({
    
    leaflet() %>%
      # addProviderTiles(providers$Hydda.Base, group = 'Base') %>%
      setView(1.519410, 41.720509, zoom = 8) %>%
      addPolygons(
        data = municipis, group = 'Municipis',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMMUNI, layerId = ~NOMMUNI,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = comarques, group = 'Comarques',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMCOMAR, layerId = ~NOMCOMAR,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = vegueries, group = 'Vegueries',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMVEGUE, layerId = ~NOMVEGUE,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = provincies, group = 'Provincies',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMPROV, layerId = ~NOMPROV,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addLayersControl(
        baseGroups = c('Provincies', 'Vegueries',
                       'Comarques', 'Municipis'),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  # popup observer
  observe({
    # event
    event <- input$baseMap_shape_click
    
    # popup content and leaflet proxy function
    parcela_popup <- function(id, lat, lng) {
      # popup_content <- as.character(
      #   tagList(
      #     tags$h4('Parcela #', data_sel_site[['idparcela']]),
      #     tags$p('altitud (m): ', data_sel_site[['altitud']]),
      #     tags$p('pendent (%): ', data_sel_site[['pendentpercentatge']]),
      #     tags$p('nivell de protecció: ', data_sel_site[['proteccio']]),
      #     tags$p('nom bosc: ', data_sel_site[['nomforest']])
      #   )
      # )
      
      popup_content <- as.character(
        tagList(
          h4(paste0('Parcela: #', data_sel_site[['idparcela']])),
          strong(sprintf('altitud: %1.f m', data_sel_site[['altitud']])),
          br(),
          strong(sprintf('pendent: %1.f %%', data_sel_site[['pendentpercentatge']])),
          br(),
          strong(sprintf('nivell de protecció: %s', data_sel_site[['proteccio']]))
        )
      )
      
      leafletProxy('baseMap') %>%
        addPopups(lng, lat, popup_content, layerId = id)
    }
    
    # check if event is null
    if (is.null(event)) {
      return()
    } else {
      # check if parcela or area
      if (!is.na(as.numeric(event$id))) {
        # data
        data_sel_site <- data_parcelas() %>%
          filter(idparcela == event$id)
        isolate(parcela_popup(event$id, event$lat, event$lng))
      } else {
        return()
      }
    }
    
  })
  
  # observer for color and size of plot circles
  observe({
    
    # variables and data
    color_var <- color()
    size_var <- size()
    reverse <- inv_pal()
    data_par <- data_parcelas()
    
    # color vector and palette
    if (color_var == '') {
      color_vector <- rep('parcela', nrow(data_par))
      pal <- colorFactor('viridis', color_vector)
    } else {
      color_vector <- data_par[[color_var]]
      pal <- colorBin('viridis', color_vector, 9, reverse = reverse)
    }
    
    # size vector
    if (size_var == '') {
      size_vector <- rep(750, nrow(data_par))
    } else {
      size_vector <- data_par[[size_var]] / max(data_par[[size_var]]) * 3000
    }
    
    # update map
    leafletProxy('baseMap', data = data_par) %>%
      addCircles(
        group = 'Parcelas', lng = ~longitude, lat = ~latitude,
        layerId = ~idparcela, stroke = FALSE, fillOpacity = 0.4,
        fillColor = pal(color_vector), radius = size_vector,
        options = pathOptions(className = 'parceladots')
      ) %>%
      addLegend(
        position = 'bottomright', pal = pal, values = color_vector,
        title = color_var, layerId = 'color_legend'
      )
  })
  
}
