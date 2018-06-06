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
    absolutePanel(
      id = 'shape_control', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, top = 520, left = 20, right = 'auto', bottom = 'auto',
      width = 330, height = 'auto',
      
      uiOutput(ns('info_shape')),
      br(),
      plotOutput(ns('donut'), height = '250px'),
      br(),
      "Qué más va aqui???",
      br(),
      textOutput(ns('debug'))
    )
  )
}

#' mod_shapeCondPanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data reactive with the data
#' @param baseMap_reactives reactive values list obtained from mod_baseMap
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
      if (click$group == 'Parcelas') {
        # parcelas
        data_parceles %>%
          filter(idparcela == click$id)
        
      } else {
        # territoris
        if (click$group == 'Provincies') {
          data_terriori <- data_parceles %>%
            filter(provincia == click$id)
        }
        
        if (click$group == 'Vegueries') {
          data_terriori <- data_parceles %>%
            filter(vegueria == click$id)
        }
        
        if (click$group == 'Comarques') {
          data_terriori <- data_parceles %>%
            filter(comarca == click$id)
        }
        
        if (click$group == 'Municipis') {
          data_terriori <- data_parceles %>%
            filter(municipi == click$id)
        }
      }
    }
  )
  
  # donut plot
  output$donut <- renderPlot({
    
    plot_data <- shape_data()
    click <- baseMap_reactives$ifn_map_shape_click
    
    if (click$group == 'Parcelas') {
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
      plot_data %>%
        ggplot(
          aes(x = idcaducesclerconif, y = percdens, fill = idcaducesclerconif)
        ) +
        geom_boxplot() +
        scale_fill_manual(name = 'densitat (%) per totas las parcel·les',
                          values = c(
                            "Conífera" = "#440154FF",
                            "Caducifoli" = "#3B528BFF",
                            "Esclerofil·le" = "#21908CFF"
        )) +
        theme_void() +
        theme(legend.position = c(.5,.5))
    }
  })
  
  # text output
  output$info_shape <- renderUI({
    
    click <- baseMap_reactives$ifn_map_shape_click
    data_sel_site <- shape_data()
    
    if (click$group == 'Parcelas') {
      tagList(
        h4(paste0('Parcela: #', data_sel_site[['idparcela']][1])),
        strong(sprintf('altitud: %1.f m', data_sel_site[['altitud']][1])),
        br(),
        strong(sprintf('pendent: %1.f %%', data_sel_site[['pendentpercentatge']][1])),
        br(),
        strong(sprintf('nivell de protecció: %s', data_sel_site[['proteccio']][1]))
      )
    } else {
      tagList(
        h4(paste0('Parcel·les en ', click$id))
      )
    }
    
  })
  
  output$debug <- renderPrint({
    baseMap_reactives$ifn_map_shape_click
  })
  
  # observer to toggle the "conditional shape panel"
  observe({
    shinyjs::toggleElement(
      id = "shape_control", anim = TRUE, animType = 'fade', time = 0.5,
      condition = !is.null(baseMap_reactives$ifn_map_shape_click) &&
        baseMap_reactives$ifn_map_shape_click$id != ""
    )
  })
  
}
