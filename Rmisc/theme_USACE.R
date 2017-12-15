theme_USACE <-  function(base_size = 8){theme(
  text = element_text(family = 'serif', color = 'black'),
  line = element_line(colour = 'black', size = 0.2), 
  rect = element_rect(colour = 'black', size = 0.2),
  plot.title = element_text(vjust = 3, size = 9),
  plot.margin = unit(c(1,1,1,1), 'lines'),
  panel.border = element_rect(fill = F),
  panel.grid.major = element_line(colour = 'grey50', size = 0.2),
  panel.grid.minor = element_line(colour = 'grey75', size = 0.1),
  panel.background = element_rect(fill = 'white'),
  #defaults legend to upper left, can/should be overridden based on graph
  legend.background = element_blank(),
  legend.justification = c("left", "top"),
  # this value should be adjusted dependent on 
  # graph with the addition of another 
  # theme(legend.position = c(X, Y)) argument after theme_USACE()
  legend.position = c(0.8, 0.5),
  legend.key = element_blank(),
  legend.title = element_text(size = 9),
  #legend.title = element_blank(), 
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(angle = 90, size = 9),
  axis.text.x = element_text(margin = margin(8, 0, 0, 0)),
  axis.text.y = element_text(margin = margin(0, 8, 0, 0)),
  axis.ticks.length = unit(0.25 , 'cm')
)}
