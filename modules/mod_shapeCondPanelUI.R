#' @title mod_shapeCondPanelUI and mod_shapeCondPanel
#' 
#' @description A shiny module to generate the base IFN plots map
#' 
#' @param id shiny id
#' 
#' @export
mod_shapeCondPanelUI <- function(id) {
  
  ns <- NS(id)
  
  # A slider-like but with tabs, to show:
  #   a) all points selected and filtered in the map
  #   b) a joined-plot based on the shape map click. For plots:
  #     1. donut plot (idcadescleconif)
  #     2. density with location for some trait (maybe % pies vivos)
  #     3. a radial plot ("rol playing character stats"-like)
  #   c) a joined-plot based on the shape map click. For polygons:
  #     1. Some plot?
  #     2. Some plot?
  #     3. Some plot?
  
  tagList(
    tabsetPanel(
      id = "tururu", type = 'pills',
      
      # tab with the shape info
      tabPanel(
        "Click",
        uiOutput(ns('info_shape')),
        br(),
        plotOutput(ns('donut'), height = '185px'),
        br(),
        "Qué más va aqui???",
        br(),
        textOutput(ns('debug'))
      ),
      
      # tab with all the info
      tabPanel(
        "Mapa completo"
      )
    )
  )
}

#' mod_shapeCondPanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data reactive with the data from dataReactive module
#' @param map_inputs reactive input values from mod_baseMap module
#' 
#' @export
#' 
#' @rdname mod_shapeCondPanel
mod_shapeCondPanel <- function(
  input, output, session,
  data, map_inputs
) {
  
  # reactive to get the data to generate the conditional panel
  shape_data <- eventReactive(
    eventExpr = map_inputs$shape_click,
    valueExpr = {
      
      click <- map_inputs$shape_click
      
      data_parceles <- data()
      if (click$group == 'Parcelas') {
        # parcelas
        data_parceles %>%
          filter(idparcela == click$id)
        
      } else {
        # territoris
        if (click$group == 'Provincies') {
          data_parceles %>%
            filter(provincia == click$id)
        } else {
          if (click$group == 'Vegueries') {
            data_parceles %>%
              filter(vegueria == click$id)
          } else {
            if (click$group == 'Comarques') {
              data_parceles %>%
                filter(comarca == click$id)
            } else {
              if (click$group == 'Municipis') {
                data_parceles %>%
                  filter(municipi == click$id)
              }
            }
          }
        }
      }
    }
  )
  
  # donut plot
  output$donut <- renderPlot({
    
    plot_data <- shape_data()
    click <- map_inputs$shape_click
    
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
    
    click <- map_inputs$shape_click
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
        h4(paste0(
          length(unique(data_sel_site[['idparcela']])),
          ' parcel·les en ', click$id
        ))
        
      )
    }
    
  })
  
  output$debug <- renderPrint({
    map_inputs$shape_click
  })
  
  # # observer to hide the cond panel if baseMap is clicked
  # observeEvent(
  #   eventExpr = map_inputs$map_click,
  #   handlerExpr = {
  #     shinyjs::hideElement(
  #       "cond_shape", anim = TRUE, animType = 'fade', time = 0.5
  #     )
  #   }
  # )
  
}
