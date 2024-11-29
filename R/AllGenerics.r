#' category-item network plot
#' 
#' @rdname cnetplot
#' @param x input object
#' @param layout network layout
#' @param showCategory selected category to be displayed
#' @param color_category color of category node
#' @param size_category relative size of the category
#' @param color_item color of item node
#' @param size_item relative size of the item (e.g., genes)
#' @param color_edge color of edge, e.g., "black". If `color = "category"`, then edges will be colored based on the category information.
#' @param size_edge relative size of edge
#' @param node_label one of 'all', 'none', 'category', 'item', 'exclusive' or 'share'
#' @param foldChange numeric values to color the item (e.g, foldChange of gene expression values)
#' @param hilight selected category to be highlighted
#' @param hilight_alpha transparent value for not selected to be highlight
#' @param ... additional parameters
#' @export
cnetplot <- function(
        x, layout = igraph::layout_nicely,
        showCategory = 5,
        color_category= "#E5C494", size_category = 1, 
        color_item = "#B3B3B3", size_item = 1, 
        color_edge = "grey", size_edge=.5,
        node_label = "all", 
        foldChange = NULL,
        hilight = "none",
        hilight_alpha = .3,
        ...
    ) {

    UseMethod("cnetplot", x)
}
