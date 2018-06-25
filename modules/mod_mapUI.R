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
  observe({
    
    admin_div <- mod_data$admin_div
    
    if (admin_div == '') {
      leafletProxy('map') %>%
        clearGroup('vegueria') %>%
        clearGroup('comarca') %>%
        clearGroup('municipi') %>%
        clearGroup('provincia')
    } else {
      if (admin_div == 'provincia') {
        leafletProxy('map') %>%
          clearGroup('vegueria') %>%
          clearGroup('comarca') %>%
          clearGroup('municipi') %>%
          addPolygons(
            data = polygons_provincies, group = 'provincia',
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
              pane = 'admin_divs'
            )
          )
      } else {
        if (admin_div == 'vegueria') {
          leafletProxy('map') %>%
            clearGroup('provincia') %>%
            clearGroup('comarca') %>%
            clearGroup('municipi') %>%
            addPolygons(
              data = polygons_vegueries, group = 'vegueria',
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
                pane = 'admin_divs'
              )
            )
        } else {
          if (admin_div == 'comarca') {
            leafletProxy('map') %>%
              clearGroup('vegueria') %>%
              clearGroup('provincia') %>%
              clearGroup('municipi') %>%
              addPolygons(
                data = polygons_comarques, group = 'comarca',
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
                  pane = 'admin_divs'
                )
              )
          } else {
            if (admin_div == 'municipi') {
              leafletProxy('map') %>%
                clearGroup('vegueria') %>%
                clearGroup('comarca') %>%
                clearGroup('provincia') %>%
                addPolygons(
                  data = polygons_municipis, group = 'municipi',
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
                    pane = 'admin_divs'
                  )
                )
            }
          }
        }
      }
    }
    
    
  })
  
  # observer for proteccions polygons, same as above
  observe({
    
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
      if (espai_tipus == 'nomein') {
        leafletProxy('map') %>%
          clearGroup('enpes') %>%
          clearGroup('nomxarxa2000') %>%
          addPolygons(
            data = polygons_pein, group = 'nomein',
            weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fill = TRUE,
            label = ~nom,
            layerId = nom_pein,
            color = '#6C7A89FF', fillColor = "#6C7A89FF",
            highlightOptions = highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE,
              fill = TRUE, fillColor = "#CF000F00"
            ),
            options = pathOptions(
              pane = 'proteccions'
            )
          )
      } else {
        if (espai_tipus == 'enpes') {
          leafletProxy('map') %>%
            clearGroup('nomein') %>%
            clearGroup('nomxarxa2000') %>%
            addPolygons(
              data = polygons_enpe, group = 'enpes',
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fill = TRUE,
              label = ~nom,
              layerId = nom_enpe,
              color = '#6C7A89FF', fillColor = "#6C7A89FF",
              highlightOptions = highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE,
                fill = TRUE, fillColor = "#CF000F00"
              ),
              options = pathOptions(
                pane = 'proteccions'
              )
            )
        } else {
          if (espai_tipus == 'nomxarxa2000') {
            leafletProxy('map') %>%
              clearGroup('nomein') %>%
              clearGroup('enpes') %>%
              addPolygons(
                data = polygons_xn2000, group = 'nomxarxa2000',
                weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fill = TRUE,
                label = ~nom_n2,
                layerId = nom_xn2000,
                color = '#6C7A89FF', fillColor = "#6C7A89FF",
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
      }
    }
  })
  
  # observer for visual candy in the map (color and size of parceles)
  observe({

    # stuff needed
    color_var <- mod_viz$color
    mida_var <- mod_viz$mida
    inverse_pal <- mod_viz$inverse_pal
    
    vars_sel <- quos(
      !!sym(color_var), !!sym(mida_var),
      !!sym('latitude'), !!sym('longitude'), !!sym('idparcela')
    )

    # check for any empty (color or mida) and remove it from the quosures
    vars_sel <- vars_sel[!vapply(vars_sel, rlang::quo_is_missing, logical(1))]
    
    data_parceles <- mod_data$data_core() %>%
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
        mida_vector <- mida_var_values / max(mida_var_values) * 3000
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
  })
  
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
