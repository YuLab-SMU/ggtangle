#' layer to draw edges of a network interactively
#'
#' @param mapping aesthetic mapping, default is NULL
#' @param data data to plot, default is NULL
#' @param geom geometric layer to draw lines
#' @param ... additional parameter passed to 'geom'
#' @return line segments layer
#' @export
#' @examples
#' library(ggiraph)
#' flow_info <- data.frame(from = LETTERS[c(1,2,3,3,4,5,6)],
#'                         to = LETTERS[c(5,5,5,6,7,6,7)])
#'
#' dd <- data.frame(
#'     label = LETTERS[1:7],
#'     v1 = abs(rnorm(7)),
#'     v2 = abs(rnorm(7)),
#'     v3 = abs(rnorm(7))
#' )
#'
#' g = igraph::graph_from_data_frame(flow_info)
#'
#' p <- ggplot(g)  + geom_edge_interactive()
#' library(ggplot2)
#' library(scatterpie)
#'
#' p2 <- p %<+% dd +
#'     geom_scatterpie(cols = c("v1", "v2", "v3")) +
#'     geom_text_interactive(aes(label=label, tooltip = label, data_id = label), nudge_y = .2) +
#'     coord_fixed()
#' 
#' girafe(ggobj = p2)
geom_edge_interactive <- function(mapping=NULL, data=NULL, geom = ggiraph::geom_segment_interactive, ...) {
    mapping <- aes(data_id = !!rlang::sym(".edge_id"))
    geom_edge(mapping = mapping, data = data, geom = geom, ...)
}


#' layer to draw edge labels of a network interactively
#'
#' @param mapping aesthetic mapping, default is NULL
#' @param data data to plot, default is NULL
#' @param geom geometric layer to draw text, default is geom_text
#' @param angle_calc how to calculate angle ('along' or 'none')
#' @param label_dodge dodge distance
#' @param ... additional parameter passed to 'geom'
#' @return text layer
#' @export
geom_edge_text_interactive <- function(
    mapping=NULL, data=NULL, 
    geom = ggiraph::geom_text_interactive, 
    angle_calc = "none", 
    label_dodge = NULL, 
    ...) {
    mapping <- aes(data_id = !!rlang::sym(".edge_id"))
    geom_edge_text(
        mapping = mapping, 
        data = data, 
        geom = geom, 
        angle_calc = angle_calc, 
        label_dodge = label_dodge, 
        ...)
}
