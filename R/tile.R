#'@importFrom ggplot2 theme element_text
tile <- function(p, x_size = 8, y_size = 2){
    p + theme(axis.text.x = element_text(size = x_size),
              axis.text.y = element_text(size = y_size),
              panel.grid = element_blank())
}
