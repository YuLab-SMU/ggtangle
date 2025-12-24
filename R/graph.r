

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
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
get_edge_data <- function(g) {
    # Use names=FALSE to get integer indices, avoiding ambiguity with non-unique labels
    e <- as.data.frame(as_edgelist(g, names = FALSE))
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
    
    # Match based on indices
    # e[,1] and e[,2] are 1-based indices from as_edgelist(g, names=FALSE)
    # d is the layout dataframe, assumed to be in same order as V(g)
    # We can match by row index directly.
    
    # Note: If layout_data reordered nodes, this assumption breaks.
    # But standard layout functions usually return coordinates for V(g) in order.
    # d should have row names 1:N or we rely on row index.
    
    # d1 corresponds to e[,1] (from)
    # d2 corresponds to e[,2] (to)
    
    # Check if indices are valid
    if (max(e[,1], e[,2]) > nrow(d)) {
         stop("Edge indices exceed node data rows. Mismatch between graph and layout data.")
    }
    
    d1 <- d[e[,1], c("x", "y")]
    d2 <- d[e[,2], c("x", "y")]
    
    names(d2) <- c("x2", "y2")
    dd <- cbind(d1, d2)
    edge_data <- cbind(e, dd)
    return(edge_data)
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
    
    # Filter params for geom_segment (remove custom ones if any remained)
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
