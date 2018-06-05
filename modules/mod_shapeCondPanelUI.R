#' @title mod_shapeCondPanelUI and mod_shapeCondPanel
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @export
mod_shapeCondPanelUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    conditionalPanel(
      condition = "input.baseMap_shape_click != ''",
      
      absolutePanel(
        id = 'shape_control', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 520, left = 20, right = 'auto', bottom = 'auto',
        width = 330, height = 'auto',
        
        h4('Inspector de parcel·les/territoris'),
        br(),
        textOutput(ns('nom_shape')),
        br(),
        plotOutput(ns('donut'), height = '100px'),
        br(),
        "Qué más va aqui???"
      )
    )
  )
}

#' mod_shapeCondPanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data reactive with the data
#' 
#' @export
#' 
#' @rdname mod_shapeCondPanel
mod_shapeCondPanel <- function(
  input, output, session,
  data
) {
  
  # reactive to get the data to generate the conditional panel
  shape_data <- eventReactive(
    eventExpr = input$baseMap_shape_click,
    valueExpr = {
      
      event <- input$baseMap_shape_click
      data_parceles <- data()
      if (!is.na(as.numeric(event$id))) {
        # parcelas
        data_parceles %>%
          filter(id_parcela == event$id)
        
      } else {
        # territoris
        if (event$id %in% unique(data_parcelas[['provincia']])) {
          data_terriori <- data_parceles %>%
            filter(provincia == event$id)
        }
        
        if (event$id %in% unique(data_parcelas[['vegueria']])) {
          data_terriori <- data_parceles %>%
            filter(vegueria == event$id)
        }
        
        if (event$id %in% unique(data_parcelas[['comarca']])) {
          data_terriori <- data_parceles %>%
            filter(comarca == event$id)
        }
        
        if (event$id %in% unique(data_parcelas[['municipi']])) {
          data_terriori <- data_parceles %>%
            filter(municipi == event$id)
        }
      }
    }
  )
  
  # donut plot
  output$donut <- renderPlot({
    
    if (length(unique(shape_data()[['idparcela']])) < 2) {
      shape_data() %>%
        ggplot(aes(x = 2, y = percdens, fill = idcaducesclerconif)) +
        geom_col() +
        coord_polar(theta = 'y') +
        scale_x_continuous(limits = c(0, 2.5)) +
        scale_fill_viridis(discrete = TRUE, end = 0.5, name = 'grup funcional') +
        theme_void() +
        theme(legend.position = c(.5,.5))
    } else {
      return()
    }
  })
  
}
