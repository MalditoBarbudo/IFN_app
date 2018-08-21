# Global script

### DB access ##################################################################
ifndb <- tidyIFN::ifn_connect(
  'guest',
  'guest',
  'ifndb'
)

## On.Stop routine #############################################################
onStop(
  function() {
    poolClose(ifndb)
  }
)

#### ggplot theme for infoPanel ################################################
theme_infoPanel <- theme_void() + theme(
  
  # line general
  # line = element_line(
  #   colour = 'black', size = 0.5, linetype = 1, lineend = 'square', arrow = NULL,
  #   
  # )
  
  # y axis (line, ticks, text)
  axis.line.y = element_line(
    colour = 'black', size = 1, linetype = 1, lineend = 'square',
    inherit.blank = FALSE
  ),
  axis.ticks.y = element_line(
    colour = 'black', size = 1, linetype = 1, lineend = 'square',
    inherit.blank = FALSE
  ),
  axis.ticks.length = unit(2, 'mm'),
  axis.text.y = element_text(
    family = 'sans', colour = 'black', size = 11, hjust = 0.5, vjust = 0.5,
    angle = 0, lineheight = 0.9, margin = margin(0, 2.2, 0, 0),
    inherit.blank = FALSE
  ),
  
  # x axis (only text)
  axis.text.x = element_text(
    family = 'sans', colour = 'black', size = 11, hjust = 0.5, vjust = 0.5,
    angle = 45, lineheight = 0.9, margin = margin(2.2, 0, 0, 0),
    inherit.blank = FALSE
  ),
  
  # legend position none by default
  legend.position = 'none',
  
  # plot title and subtitle
  plot.title = element_text(
    family = 'sans', face = 'bold', colour = 'black',
    size = rel(1.2), hjust = 0.4, vjust = 1.0,
    angle = 0, lineheight = 0.9, margin = margin(0, 0, 5.5, 0),
    inherit.blank = FALSE
  ),
  
  plot.subtitle = element_text(
    family = 'sans', face = 'plain', colour = 'black',
    size = rel(1), hjust = 0.4, vjust = 0.8,
    angle = 0, lineheight = 0.9, margin = margin(0, 0, 3.2, 0),
    inherit.blank = FALSE
  )
  
)
