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
#' @param data reactive from mod_dataReactive module
#' 
#' @export
#' 
#' @rdname mod_baseMapOutput
mod_baseMap <- function(
  input, output, session,
  municipis,comarques,vegueries,provincies,
  map_controls, data
) {
  
  
  nom_comarques <- as.character(comarques@data$NOM_COMAR)
  nom_municipis <- as.character(municipis@data$NOM_MUNI)
  nom_vegueries <- as.character(vegueries@data$NOMVEGUE)
  nom_provincies <- as.character(provincies@data$NOM_PROV)
  
  # actual base map
  output$baseMap <- renderLeaflet({
    
    leaflet() %>%
      # addProviderTiles(providers$Hydda.Base, group = 'Base') %>%
      setView(1.519410, 41.720509, zoom = 8) %>%
      # map panes to put the polygons and plots
      addMapPane('territoris', zIndex = 410) %>%
      addMapPane('parceles', zIndex = 420)
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
  
  # observer for polygons. We use this instead of add polygons directly in the
  # map and control them with the default addLayersControl because some ids
  # are identical between polygon layers (i.e. Barcelona or Lleida) causing
  # some polygons to dissapear. In this way (less efficient, I'm afraid), we
  # control the polygons drawing with a classic input - observer pair, as we
  # do with the plots dots.
  observe({
    
    # variables
    territori_val <- map_controls$territori
    
    if (is.null(territori_val)) {
      return()
    }
    
    if (territori_val == 'provincies') {
      leafletProxy('baseMap') %>%
        clearGroup('Vegueries') %>%
        clearGroup('Comarques') %>%
        clearGroup('Municipis') %>%
        addPolygons(
          data = provincies, group = 'Provincies',
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fill = TRUE,
          label = ~NOM_PROV,
          layerId = nom_provincies,
          color = '#6C7A89FF', fillColor = "#CF000F00",
          highlightOptions = highlightOptions(
            color = "#CF000F", weight = 2,
            bringToFront = FALSE,
            fill = TRUE, fillColor = "#CF000F00"
          ),
          options = pathOptions(
            pane = 'territoris'
          )
        )
    } else {
      if (territori_val == 'vegueries') {
        leafletProxy('baseMap') %>%
          clearGroup('Provincies') %>%
          clearGroup('Comarques') %>%
          clearGroup('Municipis') %>%
          addPolygons(
            data = vegueries, group = 'Vegueries',
            weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fill = TRUE,
            label = ~NOMVEGUE,
            layerId = nom_vegueries,
            color = '#6C7A89FF', fillColor = "#CF000F00",
            highlightOptions = highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE,
              fill = TRUE, fillColor = "#CF000F00"
            ),
            options = pathOptions(
              pane = 'territoris'
            )
        )
      } else {
        if (territori_val == 'comarques') {
          leafletProxy('baseMap') %>%
            clearGroup('Vegueries') %>%
            clearGroup('Provincies') %>%
            clearGroup('Municipis') %>%
            addPolygons(
              data = comarques, group = 'Comarques',
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fill = TRUE,
              label = ~NOM_COMAR,
              layerId = nom_comarques,
              color = '#6C7A89FF', fillColor = "#CF000F00",
              highlightOptions = highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE,
                fill = TRUE, fillColor = "#CF000F00"
              ),
              options = pathOptions(
                pane = 'territoris'
              )
            )
        } else {
          leafletProxy('baseMap') %>%
            clearGroup('Vegueries') %>%
            clearGroup('Comarques') %>%
            clearGroup('Provincies') %>%
            addPolygons(
              data = municipis, group = 'Municipis',
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fill = TRUE,
              label = ~NOM_MUNI,
              layerId = nom_municipis,
              color = '#6C7A89FF', fillColor = "#CF000F00",
              highlightOptions = highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE,
                fill = TRUE, fillColor = "#CF000F00"
              ),
              options = pathOptions(
                pane = 'territoris'
              )
            )
        }
      }
    }
  })
  
  # observer for color and size of plot circles
  observe({
    
    # variables and data
    color_var <- map_controls$color
    size_var <- map_controls$size
    reverse <- map_controls$inverse_pal
    data_par <- data()
    
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
      clearGroup('Parcelas') %>%
      addCircles(
        group = 'Parcelas', lng = ~longitude, lat = ~latitude,
        label = ~idparcela, layerId = ~idparcela,
        stroke = FALSE, fillOpacity = 0.4,
        fillColor = pal(color_vector), radius = size_vector,
        options = pathOptions(pane = 'parceles')
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
    # shape
    baseMap_reactives$shape_click <- input$baseMap_shape_click
    # baseMap_reactives$shape_mouseover <- input$baseMap_shape_mouseover
    # baseMap_reactives$shape_mouseout <- input$baseMap_shape_mouseout
    
    # basemap
    baseMap_reactives$map_click <- input$baseMap_click
    # baseMap_reactives$map_bounds <- input$baseMap_bounds
    # baseMap_reactives$map_zoom <- input$baseMap_zoom
    # baseMap_reactives$map_center <- input$baseMap_center
  })
  
  return(baseMap_reactives)
  
}
