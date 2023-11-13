# geneSetSimilarity ----

#' Heatmap of percentage of genes shared between gene sets
#'
#' @param gene_sets A named list of gene set members
#' @return An upper triangular ggplot heatmap where tile colours indicate
#' percentage of genes shared
geneSetSimilarity <- function(gene_sets){
    df <- .geneset_to_tbl(gene_sets)


}
