#' Category-rail layout for cnetplot networks
#'
#' Places category vertices on a right-hand rail and item vertices on a
#' left-hand arc. This layout is intended for readable category-item tracing;
#' use `layout = layout_cnet_category_rail` with [cnetplot()].
#'
#' @param graph An igraph object with `.isCategory` vertex attribute.
#' @param ... Unused additional arguments.
#' @return A two-column layout matrix.
#' @export
layout_cnet_category_rail <- function(graph, ...) {
    category <- which(igraph::V(graph)$.isCategory)
    item <- setdiff(seq_len(igraph::vcount(graph)), category)
    if (length(category) == 0 || length(item) == 0) {
        return(igraph::layout_in_circle(graph, ...))
    }
    out <- matrix(0, nrow = igraph::vcount(graph), ncol = 2)
    out[category, 1] <- 1.25
    out[category, 2] <- seq(0.8, -0.8, length.out = length(category))
    item_angle <- seq(pi / 2, 3 * pi / 2, length.out = length(item) + 2)
    item_angle <- item_angle[-c(1, length(item_angle))]
    out[item, 1] <- -0.35 + 0.85 * cos(item_angle)
    out[item, 2] <- 0.95 * sin(item_angle)
    out
}
