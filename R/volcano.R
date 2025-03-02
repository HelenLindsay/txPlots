#' # note - drawConnectors determines whether geom_text_repel or geom_text is used
# max.overlaps = Inf forces all requested labels to be shown
# min.segment.length = 0 forces all lines to be drawn
# TO DO - PROPERLY CONSTRUCT LABEL COLOUR

# volcano ----
#' Make a volcano plot
#'
#' @description Modified from EnhancedVolcano for better control over labels.
#' Accepts a table of differential expression results and returns a plot of
#' log fold change on the x-axis and -log10(p-value) on the y-axis.
#' Points are coloured according to whether they meet a p-value cutoff and/or a
#' logFC cutoff.  Dashed vertical lines indicate fold change cutoff and a
#' dashed horizontal line indicates.  Currently does not do any checking that
#' inputs have the appropriate type.
#'
#'@param tt top table of differential expression, containing columns with
#'p-values and log fold changes.
#'@param xlab Label for x-axis, default "logFC"
#'@param ylab Label for y-axis, default ="-log 10 (p-value)"
#'@param x Name of column in tt containing log fold changes, default "logFC"
#'@param y Name of column in tt containing log fold changes, default "adj_pval"
#'@param colourPoints One of "labels" or "all", default "labels".  If set to
#'"labels", only the labelled points will be coloured.  If set to "all", all
#'points are coloured.  Colour is determined by parameter "col"
#'@param FCcutoff
#'@param pCutoff P-value cutoff.
#'@param pointSize Size of points, default 2
#'@param labSize
#'@param shape Shape of points, default 19 (circles)
#'@param colAlpha Transparency of points, number between 0 and 1, default 0.5
#'@param legendIconSize Size of points in legend, default 5
#'@param col Vector of colours for non-significant, up-regulated and
#'down-regulated points
#'@param labels Genes that should be labelled with text annotations
#'@param labelsFrom Name of column in tt where labels are found
#'@param ... Extra arguments passed on to ggrepel::geom_text_repel, e.g.
#'force, which controls how far labels are pushed away from points
#'(default here is 5), fontface, which controls the text style of the labels
#'(default here is "bold", use "plain" for non-bolded), etc
#'@return A ggplot object
volcano <- function(tt,
                    xlab="logFC", ylab="-log 10 (p-value)",
                    x="logFC", y="adj_pval",
                    colourPoints=c("labels", "all"),
                    FCcutoff=1, pCutoff=0.05,
                    pointSize=2, labSize=5,
                    shape=19, colAlpha=0.5,
                    legendIconSize=5,
                    col=c("darkgray", "red2","royalblue"),
                    labels=NULL, labelsFrom="name", ...){

    colourPoints <- match.arg(colourPoints)

    # Make a "Sig" column that will be used to determine colours
    tt$Sig <- "ns"
    tt$Sig[(tt[[y]] < pCutoff) & tt[[x]] > 0] <- "up"
    tt$Sig[(tt[[y]] < pCutoff) & tt[[x]] < 0] <- "down"
    tt$Sig <- factor(tt$Sig, levels = c("ns", "up", "down"))

    pCutoff = max(tt[which(tt[y] <= pCutoff), y])

    p <- ggplot(tt, aes(x = !!sym(x), y = -log10(!!sym(y)))) +
        volcano_theme() +
        guides(colour = guide_legend(override.aes = list(size=legendIconSize))) +
        geom_point(color = "grey40",
                   alpha = colAlpha,
                   shape = shape,
                   size = pointSize,
                   na.rm = TRUE) +
        geom_vline(xintercept = c(-FCcutoff, FCcutoff),
                   linetype = "dashed",
                   colour = "darkgray",
                   linewidth = 0.4) +
        geom_hline(yintercept = -log10(pCutoff),
                   linetype = "dashed",
                   colour = "darkgray",
                   linewidth = 0.4) +
        labs(x = xlab, y = ylab)

    if (! is.null(labels)){
        p <- volcano_labels(p, tt, labels, labelsFrom,
                            col, pCutoff, labSize, ...)
    }
    return(p)

}

# volcano labels ----
volcano_labels <- function(p, tt, labels, labelsFrom,
                           col, pCutoff, labSize, ...){
    tt_subs <- tt[tt[[labelsFrom]] %in% labels, ]

    # Add coloured points ----
    p <- p + geom_point(data=tt_subs,
                        aes(color = Sig),
                        size=pointSize + 0.5) +
        scale_color_manual(values=col) + #labels = legendLabels)  +
        guides(color="none")

    # Add repel labels ----
    tt_up <- tt[tt[[labelsFrom]] %in% labels & tt$Sig == "up", ]
    tt_down <- tt[tt[[labelsFrom]] %in% labels & tt$Sig == "down", ]

    # Do not allow labels to start underneath p-val cutoff
    x_range <- range(tt[[x]])
    y_range <- c(-log10(pCutoff), max(na.omit(-log10(tt[[y]]))))

    repel_args <- list(size=labSize,
                       aes(label= !! sym(labelsFrom)),
                       force=5,
                       max.overlaps=Inf,
                       min.segment.length=0,
                       fontface="bold")
    p <- p +
        do.call(ggrepel::geom_text_repel,
                utils::modifyList(repel_args,
                                  c(list(data=tt_up,
                                         xlim=c(FCcutoff, x_range[2]),
                                         ylim=y_range),
                                    list(...)))) +
        do.call(ggrepel::geom_text_repel,
                utils::modifyList(repel_args,
                                  c(list(data=tt_down,
                                         xlim=c(x_range[1], -FCcutoff),
                                         ylim=y_range),
                                    list(...))))

    return(p)
}


# volcano theme ----
volcano_theme <- function(){
    th <- theme_bw(base_size = 24) +
        theme(axis.title = element_text(size = 18),
              legend.position = "top",
              legend.key.size = unit(0.5, "cm"),
              legend.text = element_text(size = 14),
              legend.title = element_blank(),
              panel.grid = element_blank(),
              axis.line = element_line(linewidth = 0.8,
                                       colour = "black"),
              panel.border = element_blank(),
              panel.background = element_blank())
    return(th)
}
