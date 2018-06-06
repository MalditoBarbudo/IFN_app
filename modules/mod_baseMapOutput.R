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
#' @param map_controls reactives from mod_mapControl module
#' 
#' @export
#' 
#' @rdname mod_baseMapOutput
mod_baseMap <- function(
  input, output, session,
  municipis,comarques,vegueries,provincies,
  map_controls
) {
  
  # actual base map
  output$baseMap <- renderLeaflet({
    
    nom_comarques <- as.character(comarques@data$NOM_COMAR)
    nom_municipis <- as.character(municipis@data$NOM_MUNI)
    nom_vegueries <- as.character(vegueries@data$NOMVEGUE)
    nom_provincies <- as.character(provincies@data$NOM_PROV)
    
    leaflet() %>%
      # addProviderTiles(providers$Hydda.Base, group = 'Base') %>%
      setView(1.519410, 41.720509, zoom = 8) %>%
      addPolygons(
        data = municipis, group = 'Municipis',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOM_MUNI,
        layerId = nom_municipis,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = comarques, group = 'Comarques',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOM_COMAR,
        layerId = nom_comarques,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = vegueries, group = 'Vegueries',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMVEGUE,
        layerId = nom_vegueries,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = provincies, group = 'Provincies',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOM_PROV,
        layerId = nom_provincies,
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
  # observe({
  #   # event
  #   event <- input$baseMap_shape_click
  #   
  #   # popup content and leaflet proxy function
  #   parcela_popup <- function(id, lat, lng) {
  #     
  #     popup_content <- as.character(
  #       tagList(
  #         h4(paste0('Parcela: #', data_sel_site[['idparcela']][1])),
  #         strong(sprintf('altitud: %1.f m', data_sel_site[['altitud']][1])),
  #         br(),
  #         strong(sprintf('pendent: %1.f %%', data_sel_site[['pendentpercentatge']][1])),
  #         br(),
  #         strong(sprintf('nivell de protecció: %s', data_sel_site[['proteccio']][1]))
  #       )
  #     )
  #     
  #     leafletProxy('baseMap') %>%
  #       addPopups(lng, lat, popup_content, layerId = id)
  #   }
  #   
  #   # check if event is null
  #   if (is.null(event)) {
  #     return()
  #   } else {
  #     # check if parcela or area
  #     if (!is.na(as.numeric(event$id))) {
  #       # data
  #       data_sel_site <- data_parcelas() %>%
  #         filter(idparcela == event$id)
  #       isolate(parcela_popup(event$id, event$lat, event$lng))
  #     } else {
  #       return()
  #     }
  #   }
  #   
  # })
  
  # data
  data_parcelas <- reactive({
    
    ifn_sel <- map_controls$ifn
    
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
  
  # observer for color and size of plot circles
  observe({
    
    # variables and data
    color_var <- map_controls$color
    size_var <- map_controls$size
    reverse <- map_controls$inverse_pal
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
        label = ~idparcela, layerId = ~idparcela,
        stroke = FALSE, fillOpacity = 0.4,
        fillColor = pal(color_vector), radius = size_vector,
        options = pathOptions(className = 'parceladots')
      ) %>%
      addLegend(
        position = 'bottomright', pal = pal, values = color_vector,
        title = color_var, layerId = 'color_legend'
      )
  })
  
  # prepare the returning reactive values
  baseMap_reactives <- reactiveValues()
  
  # add here all the inputs from the map needed
  observe({
    baseMap_reactives$ifn_map_shape_click <- input$baseMap_shape_click
  })
  
  return(baseMap_reactives)
  
}
