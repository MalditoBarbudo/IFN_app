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
    
    if (is.null(admin_div)) {
      return()
    }
    
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
  })
  
  # observer for proteccions polygons, same as above
  observe({
    
    espai_tipus <- mod_data$espai_tipus
    if (is.null(espai_tipus)) {
      return()
    }
    
    if (espai_tipus == 'proteccio') {
      return()
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
    
    
  })
  
}
