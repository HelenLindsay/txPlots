# geneSetMembers ----
#'Plot members of genes in genes sets
#'
#'Plots a tile plot showing genes on the x-axis and gene sets on the y-axis,
#'with fill colour (black / white) indicating membership.  Genes plotting order
#'is determined by heirarchical clustering Jaccard distances.  Gene sets are
#'plotted in the provided order, i.e. the first entry in the list is plotted
#'at the top of the figure.
#'@param gene_sets A list of gene sets to plot, where names are gene set names
#'and values are gene names.
#'@param xsize x axis text size (default 2)
#'@param ysize y axis text size (default 6)
#'@author Helen Lindsay
#'@returns A ggplot tile plot, with black tiles indicating gene presence.
geneSetMembers <- function(gene_sets, xsize = 6, ysize = 2){

    df <- .geneset_to_tbl(gene_sets)

    # Order the genes by first appearance
    tt <- t(table(df[, c("gene_set", "gene")]))
    hc <- stats::hclust(dist(tt, method = "binary"))
    # Does reversing the order make the top genes plot first?
    gene_order <- rev(hc$labels[hc$order])

    df <- df %>%
        dplyr::mutate(gene = factor(gene, levels = gene_order))

    ggplot(df, aes(x = gene, y = gene_set)) +
        geom_tile() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                         vjust = 0.5, size = xsize),
              axis.text.y = element_text(size = ysize),
              panel.grid.major = element_blank()) +
        labs(x = NULL, y = NULL) +
        coord_fixed() # makes the boxes square
}

# .geneset_to_tbl ---
.geneset_to_tbl <- function(gene_sets){
    df <- tibble::tibble(gene_set = rep(gene_sets, lengths(gene_sets)),
                         gene = unlist(gene_sets))
}
