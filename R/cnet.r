#' @rdname cnetplot
#' @importFrom igraph 'V<-'
#' @importFrom ggplot2 scale_size
#' @importFrom ggrepel geom_text_repel
#' @method cnetplot list
#' @export
cnetplot.list <- function(
  x,
  layout = "nicely",
  showCategory = 5,
  color_category = "#E5C494",
  size_category = 1,
  color_item = "#B3B3B3",
  size_item = 1,
  color_edge = "grey",
  size_edge = .5,
  categorySizeBy = ~itemNum,
  node_label = "all",
  foldChange = NULL,
  fc_threshold = NULL,
  hilight = "none",
  hilight_alpha = .3,
  ...
) {
    dots <- list(...)
    if (!is.null(dots$categorySize)) {
        stop("`categorySize` is not supported; use `categorySizeBy` instead.")
    }
    dots$categorySize <- NULL

    ## `categorySizeBy` usage demonstrations
    ## x <- list(A = letters[1:10], B = letters[5:12])
    ## attr(x, "p.adjust") <- c(A = 0.01, B = 0.2)
    ## cnetplot(x, node_label = "none", categorySizeBy = ~ -log10(p.adjust))

    x_names <- names(x)
    x_attr_names <- setdiff(names(attributes(x)), c("names", "row.names", "class"))
    categoryAttr <- list()
    for (attr_name in x_attr_names) {
        attr_value <- attr(x, attr_name)
        if (!is.numeric(attr_value)) {
            next
        }
        if (!is.null(names(attr_value))) {
            attr_value <- attr_value[x_names]
        } else if (length(attr_value) == length(x)) {
            names(attr_value) <- x_names
        } else {
            next
        }
        categoryAttr[[attr_name]] <- attr_value
    }

    x <- subset_cnet_list(x, showCategory)
    originalItemNum <- setNames(vapply(x, length, FUN.VALUE = numeric(1)), names(x))

    # node_label <- match.arg(node_label, c("category", "all", "none", "item", "gene", "exclusive", "share"))
    if (length(node_label) > 1) {
        if (getOption("cnetplot_subset", default = FALSE)) {
            x <- subset_cnet_list_item(x, node_label)
            node_label <- "all"
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
        node_label <- "item"
    }

    # Filter genes by absolute fold change threshold
    if (!is.null(fc_threshold) && !is.null(foldChange)) {
        keep_genes <- names(foldChange)[abs(foldChange) > fc_threshold]
        foldChange <- foldChange[keep_genes]
        x <- lapply(x, function(gs) {
            intersect(gs, keep_genes)
        })
        # Remove empty sets
        x <- x[sapply(x, length) > 0]
    }

    categorySizeQuo <- rlang::enquo(categorySizeBy)
    categorySizeExpr <- rlang::quo_get_expr(categorySizeQuo)
    if (rlang::is_formula(categorySizeExpr)) {
        categorySizeExpr <- rlang::f_rhs(categorySizeExpr)
    }
    exprIsItemNum <- is.symbol(categorySizeExpr) && as.character(categorySizeExpr) == "itemNum"

    categoryNames <- names(x)
    categoryDf <- data.frame(
        category = categoryNames,
        itemNum = vapply(x, length, FUN.VALUE = numeric(1)),
        stringsAsFactors = FALSE
    )
    if (length(categoryAttr) > 0) {
        for (attr_name in names(categoryAttr)) {
            categoryDf[[attr_name]] <- categoryAttr[[attr_name]][categoryNames]
        }
    }

    categorySizeValues <- rlang::eval_tidy(categorySizeExpr, data = categoryDf)
    if (!is.numeric(categorySizeValues)) {
        stop("`categorySizeBy` must evaluate to a numeric vector.")
    }
    if (length(categorySizeValues) == 1) {
        categorySizeValues <- rep(categorySizeValues, nrow(categoryDf))
    }
    if (length(categorySizeValues) != nrow(categoryDf)) {
        stop("`categorySizeBy` must return a scalar or one value per category.")
    }
    if (anyNA(categorySizeValues)) {
        stop("`categorySizeBy` returned NA values.")
    }

    g <- list2graph(x)

    is_cat <- V(g)$.isCategory
    vals <- setNames(categorySizeValues, categoryNames)
    V(g)$size[is_cat] <- vals[V(g)$name[is_cat]]

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
        fc_mapping <- aes(color = I(color_item), alpha = I(.data$.hilight))
    }

    p <- do.call(ggplot, c(list(g, layout = layout), dots))

    ## restore original category size
    if (
        length(node_label) > 1 &&
            getOption("cnetplot_subset", default = FALSE)
    ) {
        if (exprIsItemNum) {
            keep_idx <- match(names(originalItemNum), p$data$label)
            keep_idx <- keep_idx[!is.na(keep_idx)]
            p$data$size[keep_idx] <- originalItemNum[p$data$label[keep_idx]]
        }
    }

    if (color_edge == "category") {
        ed <- get_edge_data(g, names = TRUE)
        names(ed)[1] <- "category"
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
        vals <- categorySizeValues
        if (
            exprIsItemNum &&
                length(node_label) > 1 &&
                getOption("cnetplot_subset", default = FALSE)
        ) {
            originalItemNum <- originalItemNum[names(x)]
            vals <- originalItemNum
        }
        vals <- vals[is.finite(vals)]
        size_breaks <- if (length(vals) > 0) {
            pretty(vals, n = min(4, length(unique(vals))))
        } else {
            NULL
        }
        if (missing(categorySizeBy) && exprIsItemNum) {
            size_name <- "size"
        } else {
            size_name <- rlang::as_label(categorySizeExpr)
        }
        p <- p +
            ggnewscale::new_scale_color() +
            geom_point(
                fc_mapping,
                data = td_filter(!.data$.isCategory),
                size = 3 * size_item
            ) +
            scale_size(
                name = size_name,
                range = c(3, 8) * size_category,
                breaks = size_breaks
            )
    }

    if (length(node_label) > 1 || node_label != "none") {
        if (
            length(node_label) > 1 ||
                node_label %in% c("exclusive", "share")
        ) {
            p <- p + geom_cnet_label(node_label = "category")
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

    layer <- do.call(geom_text_repel, params) # (aes(label=.data$label), data = d, bg.color="white", bg.r=.1)
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
