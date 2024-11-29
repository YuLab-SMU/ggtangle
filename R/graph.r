
#' @importFrom ggplot2 ggplot
#' @method ggplot igraph
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @importFrom igraph V
#' @importFrom igraph vertex_attr
#' @importFrom igraph vertex_attr_names
#' @importFrom ggfun theme_nothing
#' @export
ggplot.igraph <- function(data = NULL, 
        mapping = aes(), 
        layout = igraph::layout_nicely, 
        ..., 
        environment = parent.frame()
    ) {
    
    layout_data <- layout(data, ...)
    if(is.list(layout_data)) layout_data <- layout_data$layout
    d <- as.data.frame(layout_data) |> setNames(c("x", "y"))
    d$label <- V(data)$name
    if (is.null(d$label)) d$label <- as.character(V(data))

    vnames <- vertex_attr_names(data)
    if(length(vnames) > 0) {
        for (vattr in vnames) {
            d[[vattr]] <- vertex_attr(data, vattr)
        }
    }

    p <- ggplot(d, aes(.data$x, .data$y)) + theme_nothing() 
    # assign("graph", data, envir = p$plot_env)
    class(p) <- c("ggtangle", class(p))
    return(p)
}

#' layer to draw edges of a network
#' 
#' @param mapping aesthetic mapping, default is NULL
#' @param data data to plot, default is NULL
#' @param geom geometric layer to draw lines
#' @param ... additional parameter passed to 'geom'
#' @return line segments layer
#' @export
#' @examples 
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
#' p <- ggplot(g)  + geom_edge()
#' library(ggplot2)
#' library(scatterpie)
#' 
#' p %<+% dd + 
#'     geom_scatterpie(cols = c("v1", "v2", "v3")) +
#'     geom_text(aes(label=label), nudge_y = .2) + 
#'     coord_fixed()
#'
geom_edge <- function(mapping=NULL, data=NULL, geom = geom_segment, ...) {
    structure(
        list(
            mapping = mapping,
            data = data,
            geom = geom,
            params = list(...)
        ),
        class = "layer_edge"
    )    
}

#' @importFrom igraph as_edgelist
#' @importFrom igraph edge_attr
#' @importFrom igraph edge_attr_names
get_edge_data <- function(g) {
    e <- as.data.frame(as_edgelist(g))
    enames <- edge_attr_names(g)
    if(length(enames) > 0) {
        for (eattr in enames) {
            e[[eattr]] <- edge_attr(g, eattr)
        }
    }

    return(e)    
}


#' @importFrom ggplot2 ggplot_add
#' @importFrom utils modifyList
#' @method ggplot_add layer_edge
#' @export 
ggplot_add.layer_edge <- function(object, plot, object_name) {
    params <- object$params

    if (is.null(object$data)) {
        # e <- get_edge_data(plot$plot_env$graph)
        e <- get_edge_data(plot$plot_env$data)
    } else {
        e <- object$data
    }
    
    d <- plot$data
    d1 <- d[match(e[,1], d$label), c("x", "y")]
    d2 <- d[match(e[,2], d$label), c("x", "y")]
    names(d2) <- c("x2", "y2")
    dd <- cbind(d1, d2)
    params$data <- cbind(e, dd)
    
    default_mapping <- aes(
        x=.data$x, y=.data$y, 
        xend=.data$x2, yend=.data$y2
    )

    if (is.null(object$mapping)) {
        params$mapping <- default_mapping
    } else {
        params$mapping <- modifyList(default_mapping, object$mapping)
    }

    layer <- do.call(object$geom, params)
    ggplot_add(layer, plot, object_name)
}







