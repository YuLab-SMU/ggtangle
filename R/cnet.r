#' @rdname cnetplot
#' @importFrom igraph 'V<-'
#' @importFrom ggplot2 scale_size
#' @importFrom ggrepel geom_text_repel
#' @method cnetplot list
#' @export
cnetplot.list <- function(
    x,
    layout = igraph::layout_nicely,
    showCategory = 5,
    color_category = "#E5C494",
    size_category = 1,
    color_item = "#B3B3B3",
    size_item = 1,
    color_edge = "grey",
    size_edge = .5,
    node_label = "all",
    foldChange = NULL,
    hilight = "none",
    hilight_alpha = .3,
    ...
) {
    x <- subset_cnet_list(x, showCategory)
    cnt <- setNames(sapply(x, length), names(x))

    # node_label <- match.arg(node_label, c("category", "all", "none", "item", "gene", "exclusive", "share"))
    if (length(node_label) > 1) {
        if (getOption('cnetplot_subset', default = FALSE)) {
            x <- subset_cnet_list_item(x, node_label)
            node_label <- 'all'
        }
    } else if (
        !node_label %in%
            c("category", "all", "none", "item", "gene", "exclusive", "share")
    ) {
        if (!grepl("[><=]", node_label)) {
            stop("wrong parameter for 'node_label'")
        } else if (is.null(foldChange)) {
            stop(
                "'foldChange' should not be NULL with the 'node_label' setting"
            )
        }
    }

    if (length(node_label) == 1 && node_label == "gene") {
        node_label = "item"
    }

    g <- list2graph(x)

    V(g)$.hilight <- 1
    if (all(hilight != "none")) {
        # maybe color the edge ?

        y <- subset_cnet_list(x, hilight)
        V(g)$.hilight <- hilight_alpha
        V(g)$.hilight[V(g)$name %in% names(y)] <- 1
        V(g)$.hilight[V(g)$name %in% unlist(y)] <- 1
    }

    # V(g)$.color <- color_item
    # V(g)$.color[1:length(x)] <- color_category

    if (!is.null(foldChange)) {
        V(g)$foldChange <- foldChange[V(g)$name]
        fc_mapping <- aes(color = foldChange, alpha = I(.data$.hilight))
    } else {
        fc_mapping = aes(color = I(color_item), alpha = I(.data$.hilight))
    }

    p <- ggplot(g, layout = layout)

    ## restore original category size
    if (
        length(node_label) > 1 &&
            getOption('cnetplot_subset', default = FALSE)
    ) {
        p$data$size[match(names(cnt), p$data$label)] <- cnt
    }

    if (color_edge == "category") {
        ed <- get_edge_data(g)
        names(ed)[1] <- 'category'
        p <- p +
            geom_edge(
                aes(color = .data$category),
                data = ed,
                linewidth = size_edge
            )
    } else {
        p <- p + geom_edge(color = color_edge, linewidth = size_edge)
    }

    p <- p +
        geom_point(
            aes(size = .data$size, alpha = I(.data$.hilight)),
            data = td_filter(.data$.isCategory),
            color = color_category
        )
    if (size_item > 0) {
        p <- p +
            ggnewscale::new_scale_color() +
            geom_point(
                fc_mapping,
                data = td_filter(!.data$.isCategory),
                size = 3 * size_item
            ) +
            scale_size(
                range = c(3, 8) * size_category,
                breaks = pretty(cnt, n = min(4, diff(range(cnt))))
            )
    }

    if (length(node_label) > 1 || node_label != "none") {
        if (
            length(node_label) > 1 ||
                node_label %in% c("exclusive", "share")
        ) {
            p <- p + geom_cnet_label(node_label = 'category')
        }

        p <- p + geom_cnet_label(node_label = node_label)
    }

    return(p)
}

# maybe a geom_cnet_point function

#' @method ggplot_add cnet_label
#' @export
ggplot_add.cnet_label <- function(object, plot, object_name, ...) {
    params <- object$params
    node_label <- object$node_label
    default_params <- list(bg.color = "white", bg.r = .1)
    params <- modifyList(default_params, params)

    default_mapping <- aes(label = .data$label)
    if (is.null(object$mapping)) {
        params$mapping <- default_mapping
    } else {
        params$mapping <- modifyList(default_mapping, object$mapping)
    }

    x <- graph2list(plot$plot_env$data)

    if (!is.null(object$data)) {
        d <- object$data
    } else if (length(node_label) > 1) {
        d <- td_filter(.data$label %in% node_label)
    } else if (node_label == "all") {
        d <- NULL
    } else if (node_label == "category") {
        d <- td_filter(.data$.isCategory)
    } else if (node_label == "item") {
        d <- td_filter(!.data$.isCategory)
    } else if (node_label %in% c("exclusive", "share")) {
        node_to_label <- lapply(seq_along(x), function(i) {
            j <- x[[i]] %in% unlist(x[-i])
            if (node_label == "exclusive") {
                return(x[[i]][!j])
            } else {
                return(x[[i]][j])
            }
        })
        d <- td_filter(.data$label %in% unlist(node_to_label))
    } else {
        d <- td_filter(!!str2lang(paste("foldChange", node_label)))
    }

    params$data <- d

    layer <- do.call(geom_text_repel, params) #(aes(label=.data$label), data = d, bg.color="white", bg.r=.1)
    ggplot_add(layer, plot, object_name, ...)
}

#' add labels of cnetplot
#'
#' @title geom_cnet_label
#' @param mapping aes mapping, default is NULL
#' @param data plot data, default is NULL
#' @param node_label which type of node label to be displayed, see also [cnetplot]
#' @param ... parameters that passed to `geom_text_repel`
#' @export
#' @author Guangchuang Yu
geom_cnet_label <- function(
    mapping = NULL,
    data = NULL,
    node_label = "all",
    ...
) {
    structure(
        list(
            mapping = mapping,
            data = data,
            node_label = node_label,
            params = list(...)
        ),
        class = "cnet_label"
    )
}


subset_cnet_list <- function(x, showCategory) {
    if (!is.numeric(showCategory)) {
        return(x[names(x) %in% showCategory])
    }

    n <- length(x)
    if (length(showCategory) == 1) {
        showCategory <- seq(showCategory)
    }

    if (any(showCategory) > n) {
        msg <- sprintf(
            "any showCategory value that is large than %d will be removed.",
            n
        )
        message(msg)
    }

    showCategory <- showCategory[showCategory <= n]
    return(x[showCategory])
}

subset_cnet_list_item <- function(x, showItem = "all") {
    if (length(showItem) == 1 && showItem == "all") {
        return(x)
    }

    lapply(x, \(y) y[y %in% showItem])
}
