# Usage: p + jitterbox()
jitterbox <- function(notch = TRUE){
    list(ggplot2::geom_boxplot(outlier.shape = NA, notch = notch),
         ggplot2::geom_jitter())
}
