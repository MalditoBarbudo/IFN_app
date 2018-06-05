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
      condition = "typeof input.baseMap_shape_click !== 'undefined'",
      # condition = "1 == 1",
      absolutePanel(
        id = 'shape_control', class = 'panel panel-default', fixed = TRUE,
        draggable = TRUE, top = 520, left = 20, right = 'auto', bottom = 'auto',
        width = 330, height = 'auto',
        
        uiOutput(ns('info_shape')),
        br(),
        plotOutput(ns('donut'), height = '250px'),
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
#' @param event shape click event
#' 
#' @export
#' 
#' @rdname mod_shapeCondPanel
mod_shapeCondPanel <- function(
  input, output, session,
  data, baseMap_reactives
) {
  
  # reactive to get the data to generate the conditional panel
  shape_data <- eventReactive(
    eventExpr = baseMap_reactives$ifn_map_shape_click,
    valueExpr = {
      
      click <- baseMap_reactives$ifn_map_shape_click
      
      data_parceles <- data()
      if (!is.na(as.numeric(click$id))) {
        # parcelas
        data_parceles %>%
          filter(idparcela == click$id)
        
      } else {
        # territoris
        if (click$id %in% unique(data_parcelas[['provincia']])) {
          data_terriori <- data_parceles %>%
            filter(provincia == click$id)
        }
        
        if (click$id %in% unique(data_parcelas[['vegueria']])) {
          data_terriori <- data_parceles %>%
            filter(vegueria == click$id)
        }
        
        if (click$id %in% unique(data_parcelas[['comarca']])) {
          data_terriori <- data_parceles %>%
            filter(comarca == click$id)
        }
        
        if (click$id %in% unique(data_parcelas[['municipi']])) {
          data_terriori <- data_parceles %>%
            filter(municipi == click$id)
        }
      }
    }
  )
  
  # donut plot
  output$donut <- renderPlot({
    
    plot_data <- shape_data()
    if (length(unique(plot_data[['idparcela']])) < 2) {
      plot_data %>%
        ggplot(aes(x = 2, y = percdens, fill = idcaducesclerconif)) +
        geom_col() +
        coord_polar(theta = 'y') +
        scale_x_continuous(limits = c(.5, 2.5)) +
        # scale_fill_viridis(discrete = TRUE, end = 0.5, name = 'densitat %') +
        scale_fill_manual(name = 'densitat %', values = c(
          "Conífera" = "#440154FF",
          "Caducifoli" = "#3B528BFF",
          "Esclerofil·le" = "#21908CFF"
        )) +
        theme_void() +
        theme(legend.position = c(.5,.5))
    } else {
      return()
    }
  })
  
  # text output
  output$info_shape <- renderUI({
    
    click <- baseMap_reactives$ifn_map_shape_click
    data_sel_site <- shape_data()
    
    if (length(unique(data_sel_site[['idparcela']])) < 2) {
      tagList(
        h4(paste0('Parcela: #', data_sel_site[['idparcela']][1])),
        strong(sprintf('altitud: %1.f m', data_sel_site[['altitud']][1])),
        br(),
        strong(sprintf('pendent: %1.f %%', data_sel_site[['pendentpercentatge']][1])),
        br(),
        strong(sprintf('nivell de protecció: %s', data_sel_site[['proteccio']][1]))
      )
    } else {
      return()
    }
    
  })
  
}
