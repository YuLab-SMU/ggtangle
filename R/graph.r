

#' Plot an igraph object
#' 
#' @method ggplot igraph
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @importFrom igraph V
#' @importFrom igraph vertex_attr
#' @importFrom igraph vertex_attr_names
#' @importFrom ggfun theme_nothing
#' @param data an igraph object (or a mechgraph object for the
#'   `ggplot.mechgraph` method) to be converted and plotted.
#' @param mapping default aesthetics, default is `aes()`.
#' @param layout network layout. Supported values are the layout names
#'   handled by [layout_circular], [layout_linear], [layout_fishbone] and other
#'   ggtangle/igraph layouts, or a custom layout function. Default is
#'   `"nicely"`.
#' @param ... additional parameters passed to the layout function.
#' @param environment environment in which to evaluate the mapping, default is
#'   `parent.frame()`.
#' @export
ggplot.igraph <- function(data = NULL, 
        mapping = aes(), 
        layout = "nicely", 
        ..., 
        environment = parent.frame()
    ) {
    
    layout <- get_igraph_layout(layout)
    layout_data <- layout(data, ...)
    if(is.list(layout_data)) layout_data <- layout_data$layout
    d <- as.data.frame(layout_data) |> setNames(c("x", "y"))
    d$label <- V(data)$name
    if (is.null(d$label)) d$label <- as.character(V(data))
    
    # Store ID if available, or create one
    # igraph usually doesn't have an 'id' attribute by default unless set.
    # But V(data) is indexable.
    # We need a reliable way to match edge source/target to node coordinates.
    # If d$label is used for matching, it must be unique.
    # If labels are not unique (e.g. multiple "Man" nodes), matching by label fails.
    # This is the critical bug identified by the user.
    
    # We should add an explicit internal ID column.
    d$.ggtangle_id <- as.character(seq_len(nrow(d)))
    
    # If original graph had names, use them?
    # But edge list refers to names if present, or indices if not?
    # igraph::as_edgelist returns names if V(g)$name exists, otherwise indices.
    # Let's ensure we use indices for matching to be safe against non-unique labels.
    
    vnames <- vertex_attr_names(data)
    if(length(vnames) > 0) {
        for (vattr in vnames) {
            d[[vattr]] <- vertex_attr(data, vattr)
        }
    }

    p <- ggplot(d, aes(.data$x, .data$y)) + theme_nothing() 
    
    assign("graph", data, envir = p$plot_env) 
    
    class(p) <- c("ggtangle", class(p))
    return(p)
}

#' @rdname ggplot.igraph
#' @method ggplot mechgraph
#' @importFrom igraph as.igraph
#' @export
ggplot.mechgraph <- function(data = NULL, mapping = aes(), layout = "nicely",
                             ..., environment = parent.frame()) {
    g <- igraph::as.igraph(data)
    ggplot.igraph(g, mapping = mapping, layout = layout, ...,
                  environment = environment)
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

#' layer to draw edge labels of a network
#' 
#' @param mapping aesthetic mapping, default is NULL
#' @param data data to plot, default is NULL
#' @param geom geometric layer to draw text, default is geom_text
#' @param angle_calc how to calculate angle ('along' or 'none')
#' @param label_dodge dodge distance
#' @param ... additional parameter passed to 'geom'
#' @return text layer
#' @export
geom_edge_text <- function(mapping=NULL, data=NULL, geom = geom_text, angle_calc = "none", label_dodge = NULL, ...) {
    structure(
        list(
            mapping = mapping,
            data = data,
            geom = geom,
            params = list(angle_calc = angle_calc, label_dodge = label_dodge, ...)
        ),
        class = "layer_edge_text"
    )    
}


#' @importFrom igraph as_edgelist
#' @importFrom igraph edge_attr
#' @importFrom igraph edge_attr_names
#' @importFrom igraph V
get_edge_data <- function(g, names = FALSE) {
    # Use names=FALSE to get integer indices, avoiding ambiguity with non-unique labels
    e <- as.data.frame(as_edgelist(g, names = names))
    enames <- edge_attr_names(g)
    if(length(enames) > 0) {
        for (eattr in enames) {
            e[[eattr]] <- edge_attr(g, eattr)
        }
    }

    return(e)    
}

# Helper to prepare edge data with coordinates
get_edge_plot_data <- function(object, plot) {
    if (is.null(object$data)) {
        if (exists("graph", envir = plot$plot_env)) {
            g <- get("graph", envir = plot$plot_env)
            e <- get_edge_data(g)
        } else {
            stop("Graph object not found. Ensure plot was created with ggplot(graph_object).")
        }
    } else {
        e <- object$data
    }
    
    d <- plot$data
    
    # Check if edge list uses names (character/factor) or indices (numeric)
    if (is.character(e[,1]) || is.factor(e[,1])) {
        if (is.null(d$label)) {
            stop("Layout data missing 'label' column, cannot match named edges.")
        }
        idx1 <- match(as.character(e[,1]), as.character(d$label))
        idx2 <- match(as.character(e[,2]), as.character(d$label))
        
        if (any(is.na(idx1)) | any(is.na(idx2))) {
            stop("Some edge names not found in layout labels.")
        }
    } else {
        # Match based on indices
        # e[,1] and e[,2] are 1-based indices from as_edgelist(g, names=FALSE)
        idx1 <- e[,1]
        idx2 <- e[,2]
        
        # Check if indices are valid
        if (max(idx1, idx2) > nrow(d)) {
             stop("Edge indices exceed node data rows. Mismatch between graph and layout data.")
        }
    }
    
    d1 <- d[idx1, c("x", "y")]
    d2 <- d[idx2, c("x", "y")]
    
    names(d2) <- c("x2", "y2")
    dd <- cbind(d1, d2)
    edge_data <- cbind(e, dd)
    if (.check_interactive_attr(object)){
        edge_data$`.edge_id` <- paste0(e[,1], "_", e[,2]) 
    }
    return(edge_data)
}

#' Construct ggraph-compatible cubic Bezier control points for circular edges
#'
#' The old ggraph `geom_edge_arc()` uses two control points on the radial
#' segments from each endpoint toward the layout centre. This is the circular
#' branch of ggraph's `create_arc()` and is intentionally kept here so the
#' ggtangle backend can reproduce that geometry without depending on ggraph.
#'
#' @noRd
circular_bezier_edges <- function(edge_data) {
    if (is.null(edge_data) || nrow(edge_data) == 0) return(edge_data)
    cx <- mean(c(edge_data$x, edge_data$x2))
    cy <- mean(c(edge_data$y, edge_data$y2))
    x0 <- edge_data$x - cx
    y0 <- edge_data$y - cy
    x1 <- edge_data$x2 - cx
    y1 <- edge_data$y2 - cy
    dx <- x1 - x0
    dy <- y1 - y0
    half_dist <- sqrt(dx^2 + dy^2) / 2
    r0 <- sqrt(x0^2 + y0^2)
    r1 <- sqrt(x1^2 + y1^2)
    f0 <- ifelse(r0 > 0, 1 - half_dist / r0, 1)
    f1 <- ifelse(r1 > 0, 1 - half_dist / r1, 1)
    out <- data.frame(
        x = c(edge_data$x, cx + x0 * f0, cx + x1 * f1, edge_data$x2),
        y = c(edge_data$y, cy + y0 * f0, cy + y1 * f1, edge_data$y2),
        group = rep(seq_len(nrow(edge_data)), 4)
    )
    out <- out[order(out$group, rep(1:4, each = nrow(edge_data))), , drop = FALSE]
    extras <- edge_data[rep(seq_len(nrow(edge_data)), each = 4),
        setdiff(names(edge_data), c("x", "y", "x2", "y2")), drop = FALSE]
    cbind(out, extras)
}

#' Reverse edge endpoints so a single curvature value bows every edge outward
#'
#' `geom_curve()` draws all edges of a layer with one signed curvature, which
#' makes a radial layout look like a uniform pinwheel. For an edge with
#' direction `d = (dx, dy)` and outward vector `out = midpoint - centroid`,
#' positive curvature bulges toward the clockwise-perpendicular of `d`,
#' i.e. `(dy, -dx)`. Flipping the endpoints negates `d`, hence the bulge side,
#' so edges with `(dy, -dx) %*% out < 0` are reversed to bulge outward.
#'
#' @param edge_data data.frame with columns `x`, `y`, `x2`, `y2`
#' @return data.frame with some rows' endpoints swapped
#' @noRd
orient_edge_curvature <- function(edge_data) {
    if (is.null(edge_data) || nrow(edge_data) == 0) return(edge_data)
    cx <- mean(c(edge_data$x, edge_data$x2))
    cy <- mean(c(edge_data$y, edge_data$y2))
    dx <- edge_data$x2 - edge_data$x
    dy <- edge_data$y2 - edge_data$y
    out_x <- (edge_data$x + edge_data$x2) / 2 - cx
    out_y <- (edge_data$y + edge_data$y2) / 2 - cy
    dot <- dy * out_x - dx * out_y
    flip <- dot < 0 & (dx^2 + dy^2 > 0)
    flip[is.na(flip)] <- FALSE
    if (any(flip)) {
        tmp_x <- edge_data$x[flip]
        tmp_y <- edge_data$y[flip]
        edge_data$x[flip] <- edge_data$x2[flip]
        edge_data$y[flip] <- edge_data$y2[flip]
        edge_data$x2[flip] <- tmp_x
        edge_data$y2[flip] <- tmp_y
    }
    edge_data
}

#' @importFrom ggplot2 ggplot_add
#' @importFrom utils modifyList
#' @method ggplot_add layer_edge
#' @export 
ggplot_add.layer_edge <- function(object, plot, object_name, ...) {
    params <- object$params
    edge_data <- get_edge_plot_data(object, plot)
    params$data <- edge_data
    
    default_mapping <- aes(
        x=.data$x, y=.data$y, 
        xend=.data$x2, yend=.data$y2
    )

    if (is.null(object$mapping)) {
        params$mapping <- default_mapping
    } else {
        params$mapping <- modifyList(default_mapping, object$mapping)
    }
    
    # A Bezier edge layer receives four control points per edge. This is the
    # circular arc geometry used by the former ggraph backend.
    if (identical(object$geom, ggfun::geom_bezier) ||
        identical(object$geom, ggfun::GeomBezier)) {
        params$data <- circular_bezier_edges(edge_data)
        params$mapping <- aes(x = .data$x, y = .data$y, group = .data$group)
    } else if (!is.null(params$curvature)) {
        # Backward-compatible geom_curve path for callers that explicitly
        # supply another curved geom.
        len2 <- (edge_data$x2 - edge_data$x)^2 + (edge_data$y2 - edge_data$y)^2
        params$data <- edge_data[is.na(len2) | len2 > 0, , drop = FALSE]
        params$data <- orient_edge_curvature(params$data)
    }

    special_params <- c("angle_calc", "label_dodge")
    geom_params <- params[!names(params) %in% special_params]
    
    layer <- do.call(object$geom, geom_params)
    ggplot_add(layer, plot, object_name, ...)
}

#' @method ggplot_add layer_edge_text
#' @export 
#' @importFrom rlang sym
ggplot_add.layer_edge_text <- function(object, plot, object_name, ...) {
    params <- object$params
    edge_data <- get_edge_plot_data(object, plot)
    
    # Calculate midpoints and angles
    lbl_data <- edge_data
    lbl_data$x_mid <- (lbl_data$x + lbl_data$x2) / 2
    lbl_data$y_mid <- (lbl_data$y + lbl_data$y2) / 2
    
    if (!is.null(params$angle_calc) && params$angle_calc == "along") {
        ang <- atan2(lbl_data$y2 - lbl_data$y, lbl_data$x2 - lbl_data$x) * 180 / pi
        # Normalize angle to [-90, 90] for readability
        lbl_data$angle <- ifelse(ang > 90, ang - 180, ifelse(ang < -90, ang + 180, ang))
    } else {
        lbl_data$angle <- 0
    }
    
    # Dodge (placeholder logic)
    # if (!is.null(params$label_dodge)) { ... }
    
    params$data <- lbl_data
    
    default_mapping <- aes(x = !!sym("x_mid"), y = !!sym("y_mid"))
    
    # Auto map angle if calculated
    if (!is.null(params$angle_calc) && params$angle_calc == "along") {
        # Check if user already mapped angle
        if (is.null(object$mapping) || !"angle" %in% names(object$mapping)) {
             default_mapping <- modifyList(default_mapping, aes(angle = !!sym("angle")))
        }
    }

    if (is.null(object$mapping)) {
        params$mapping <- default_mapping
    } else {
        params$mapping <- modifyList(default_mapping, object$mapping)
    }
    
    # Remove custom params before calling geom
    special_params <- c("angle_calc", "label_dodge")
    geom_params <- params[!names(params) %in% special_params]
    
    layer <- do.call(object$geom, geom_params)
    ggplot_add(layer, plot, object_name, ...)
}

.check_interactive_attr <- function(x){
    attrs <- c("tooltip", "data_id", "onclick")
    flag <- any(names(x$mapping) %in% attrs)
    return(flag)
}
