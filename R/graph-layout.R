#' @importFrom rlang caller_env
#' @importFrom yulab.utils yulab_abort
as_igraphlayout <- function(type, call = caller_env()) {
    if (type %in% igraph_layouts) {
        layout <- type
    } else {
        new_type <- sprintf("%s_%s", c("as", "in", "with", "on"), type)
        i <- which(new_type %in% igraph_layouts)
        if (length(i) == 0) {
            yulab_abort("Cannot find the igraph layout {.val {type}}", call = call)
        }
        layout <- new_type[i]
    }

    sprintf("layout_%s", layout)
}

is_igraphlayout <- function(type) {
    if (type %in% igraph_layouts) {
        return(TRUE)
    } else {
        new_type <- sprintf("%s_%s", c("as", "in", "with", "on"), type)
        if (any(new_type %in% igraph_layouts)) {
            return(TRUE)
        }
    }

    return(FALSE)
}

#' @importFrom yulab.utils get_fun_from_pkg
get_igraph_layout <- function(layout) {
    if (is.function(layout)) {
        return(layout)
    }
    if (is_igraphlayout(layout)) {
        layout <- as_igraphlayout(layout)
    }

    if (layout == "layout_circular") {
        return(layout_circular)
    }

    if (layout == "layout_linear") {
        return(layout_linear)
    }

    get_fun_from_pkg("igraph", layout)
}

#' Circular layout
#'
#' @param graph A graph object.
#' @param sort.by The attribute to sort the nodes by. Default is NULL.
#' @param ... Additional arguments passed to \code{igraph::layout_in_circle}.
#' @return A matrix of coordinates.
#' @importFrom igraph layout_in_circle
#' @importFrom igraph V
#' @importFrom igraph degree
#' @export
#' @author Guangchuang Yu
layout_circular <- function(graph, sort.by = NULL, ...) {
    if (is.null(sort.by)) {
        return(layout_in_circle(graph, ...))
    }

    if (sort.by == "degree") {
        v <- degree(graph)
    } else {
        v <- vertex_attr(graph, sort.by)
    }

    if (is.null(v)) {
        warning("sort.by attribute not found, using default order")
        return(layout_in_circle(graph, ...))
    }

    ord <- order(v, decreasing = TRUE)
    layout_in_circle(graph, order = ord, ...)
}

#' Linear layout
#'
#' @param graph A graph object.
#' @param sort.by The attribute to sort the nodes by. Default is NULL.
#' @param ... Additional arguments.
#' @return A matrix of coordinates.
#' @export
#' @author Guangchuang Yu
layout_linear <- function(graph, sort.by = NULL, ...) {
    if (is.null(sort.by)) {
        N <- length(V(graph))
        return(matrix(c(1:N, rep(0, N)), ncol = 2))
    }

    if (sort.by == "degree") {
        v <- degree(graph)
    } else {
        v <- vertex_attr(graph, sort.by)
    }

    if (is.null(v)) {
        warning("sort.by attribute not found, using default order")
        N <- length(V(graph))
        return(matrix(c(1:N, rep(0, N)), ncol = 2))
    }

    ord <- order(v, decreasing = TRUE)
    x <- numeric(length(ord))
    x[ord] <- 1:length(ord)

    matrix(c(x, rep(0, length(x))), ncol = 2)
}


igraph_layouts <- c(
    "as_bipartite",
    "as_star",
    "as_tree",
    "in_circle",
    "nicely",
    "with_dh",
    "with_drl",
    "with_gem",
    "with_graphopt",
    "on_grid",
    "with_mds",
    "with_sugiyama",
    "on_sphere",
    "randomly",
    "with_fr",
    "with_kk",
    "with_lgl",
    "circular",
    "linear"
)
