pitch_plot <- function(width, length, line_color = 'black'){
  library(ggplot2)
  
  # the middle of a pitch is a (0,0) point
  # x refers to coordinate along football pitch length, y - along width
  
  p <- ggplot() +  
    geom_rect(aes(xmin = -length/2, xmax = length/2, ymin = -width/2, ymax = width/2), fill = NA, color = line_color) +
    # goals
    geom_rect(aes(xmin = -length/2 - 1, xmax = -length/2, ymin = -3.66, ymax = 3.66), fill = NA, color = line_color) +
    geom_rect(aes(xmin = length/2, xmax = length/2 + 1, ymin = -3.66, ymax = 3.66), fill = NA, color = line_color) +
    # penalty areas
    geom_rect(aes(xmin = -length/2, xmax = -length/2 + 16.5, ymin = -20.16, ymax = 20.16), fill = NA, color = line_color) +
    geom_rect(aes(xmin = length/2 - 16.5, xmax = length/2, ymin = -20.16, ymax = 20.16), fill = NA, color = line_color) +
    # goal areas
    geom_rect(aes(xmin = -length/2, xmax = -length/2 + 5.5, ymin = -9.16, ymax = 9.16), fill = NA, color = line_color) +
    geom_rect(aes(xmin = length/2 - 5.5, xmax = length/2, ymin = -9.16, ymax = 9.16), fill = NA, color = line_color) +
    # half-way line
    geom_segment(aes(x = 0, xend = 0, y = -width/2, yend = width/2), color = line_color) +
    # centre circle 
    geom_path(data = data.frame(x = c(-9150:(-1)/1000, 1:9150/1000),
                                y = c(sqrt(9.15^2 - c(-9150:(-1)/1000, 1:9150/1000)^2))), 
              aes(x, y), color = line_color) +
    geom_path(data = data.frame(x = c(-9150:(-1)/1000, 1:9150/1000),
                                y = -c(sqrt(9.15^2 - c(-9150:(-1)/1000, 1:9150/1000)^2))), 
              aes(x, y), color = line_color) + 
    # penalty arcs
    geom_path(data = dplyr::filter(data.frame(x = -length/2 + 11 + c(sqrt(9.15^2 - c(-9150:(-1)/1000, 1:9150/1000)^2)),
                                              y = c(-9150:(-1)/1000, 1:9150/1000)),
                                   x > -length/2 + 16.5),
              aes(x, y), color = line_color) +
    geom_path(data = dplyr::filter(data.frame(x = length/2 - 11 - c(sqrt(9.15^2 - c(-9150:(-1)/1000, 1:9150/1000)^2)),
                                              y = c(-9150:(-1)/1000, 1:9150/1000)), 
                                   x < length/2 - 16.5),
              aes(x, y), color = line_color) +
    # penalty spots and centre spot
    geom_point(aes(-length/2 + 11, 0), color = line_color, size = 1) +
    geom_point(aes(length/2 - 11, 0), color = line_color, size = 1) +
    geom_point(aes(0, 0), color = line_color, size = 1) +
    # set completely empty theme
    theme_void()
  return(p)
}
