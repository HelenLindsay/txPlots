# To do - do the filtering elsewhere?
# set the plotting order here or elsewhere?

# geneSetHeatmap ----
#'Heatmap for comparing gene set results between methods
#'
#'Returns a heatmap where axes are gene sets and methods, and fill colour is
#'determined by a gene set score, e.g. a p-value.  Tiles are set to be square
#'using coord_fixed.  Default text size assumes that gene sets are plotted on
#'the x-axis
#'@param x unquoted column name for x values
#'@param y unquoted column name for y values
#'@param fill unquoted column name for tile colour
#'@param xsize x axis text size (default 2)
#'@param ysize y axis text size (default 6)
#'@param legend_lab label for the fill legend, (default "P-value")
#'@param mid Adjust the midpoint of the fill scale to change yellow/blue
#'balance of the default viridis palette. The idea behind this parameter is that
#'we may wish to devote more of the colour range to a subset of values.  For
#'p-values, we may wish to be able to distinguish small (significant) p-values,
#'but not need to be able to discriminate between larger (non-significant)
#'values (default 0.05)
#'@param top_n (integer(1)) if specified, filter to at most (top_n) gene sets,
#'where gene sets are assumed to be the y column (default NULL, i.e. do not
#'filter)
#'@importFrom ggplot2 aes ggplot element_text element_blank
#'@importFrom ggplot2 scale_fill_viridis_c theme_bw labs
#'@returns A ggplot2 heatmap with tile fill colour values set using a continous
#'viridis palette
#'@author Helen Lindsay
#'@export
geneSetHeatmap <- function(dat, x, y, fill, xsize=2, ysize=6, mid=0.05,
                           top_n=NULL){

    if (! is.null(top_n)){
        y_string <- deparse(substitute(yval))
        dat <- filterTopN(dat, y_string, top_n)
    }

    ggplot(dat, aes(x={{x}}, y={{y}}, fill={{fill}})) +
        geom_tile() +
        scale_fill_viridis_c(direction=-1, # Make small p-vals yellow not blue
                             values=scales::rescale(c(0, mid, 1)),
                             limits=c(0, 1)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust = 1,
                                         vjust = 0.5,
                                         size = xsize),
              axis.text.y = element_text(size = ysize),
              panel.grid.major = element_blank()) +
        labs(x = NULL, y = NULL, fill = legend_lab) +
        coord_fixed() # makes the boxes square
}
