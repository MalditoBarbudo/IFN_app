#' @title mod_baseMapOutput and mod_baseMap
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @param label baseMap label
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
#' 
#' @export
#' 
#' @rdname mod_baseMapOutput
mod_baseMap <- function(
  input, output, session,
  municipis, comarques, vegueries, provincies
) {
  
  output$ifn_map <- renderLeaflet({
    
    leaflet() %>%
      # addProviderTiles(providers$Hydda.Base, group = 'Base') %>%
      setView(1.519410, 41.720509, zoom = 8) %>%
      addPolygons(
        data = municipis, group = 'Municipis',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMMUNI,
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = comarques, group = 'Comarques',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMCOMAR,
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
        color = '#6C7A89FF', fillColor = "#CF000F00",
        highlightOptions = highlightOptions(color = "#CF000F", weight = 2,
                                            bringToFront = FALSE,
                                            fill = TRUE, fillColor = "#CF000F00")
      ) %>%
      addPolygons(
        data = provincies, group = 'Provincies',
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fill = TRUE,
        label = ~NOMPROV,
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
  
}
