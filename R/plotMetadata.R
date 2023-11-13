# plotMetadata ----
# To do
# - make sure that sample names are conserved across the binary, factor
# and continuous heatmaps.  Not finished yet.
# - make clustering of binary matrix optional
# - Collect colour palettes - scale_color_brewer is native to ggplot2
# https://github.com/EmilHvitfeldt/r-color-palettes
#
#'Make a heatmap a metadata table
#'
#'Use column classes to group columns by data type. Intended as a quick
#'visualisation of a clinical data table.  Binary variables
#'are those with either two unique values or logical column type and are
#'plotted in black and white.
#'@param df
#'@param id unquoted name of id column in df
#'@param discrete Cutoff of number of unique values for considering a
#'column discrete vs continuous.  Default 10 means that columns with at most
#'10 unique values are considered discrete (numeric(1), default 10).  Factor
#'columns are always treated as factors
#'@importFrom stats hclust dist
#'@importFrom ggplot2 ggplot aes geom_tile
#'@importFrom ggplot2 theme theme_bw
#'@importFrom ggplot2 element_text
plotMetadata <- function(df, id, discrete = 10){
    col_classes <- unlist(lapply(df, class))
    n_vals <- apply(df, 2, function(x) length(unique(x)))

    # To do - test for TRUE/FALSE/NA in a character column?
    is_binary <- (col_classes == "logical" | n_vals == 2) &!
        col_classes == "factor"
    is_discrete <- col_classes == "factor" | n_vals <= discrete & ! is_binary

    # Order the columns by Jaccard distance
    binary_m <- df[is_binary] %>%
        dplyr::mutate_all(as.numeric) %>%
        as.matrix()
    bin_col_ord <- hclust(dist(t(binary_m), method = "binary"))$order
    bin_cols <- colnames(df)[is_binary][bin_col_ord]

    binary_df <- df %>%
        dplyr::select({{id}}, dplyr::all_of(bin_cols)) %>%
        tidyr::pivot_longer(-{{id}})

}

.binaryHeatmap <- function(df, id, y_size = 2){
    ggplot2::ggplot(df, aes(x = name, y = {{id}}, fill = value)) +
        ggplot2::geom_tile() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x =
                           element_text(angle = 90, hjust = 1, vjust = 0.5),
                       axis.text.y = element_text(size = y_size)) +
        ggplot2::scale_fill_manual(values = c("black", "white"))
}
