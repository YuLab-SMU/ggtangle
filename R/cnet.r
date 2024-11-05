#' @rdname cnetplot
#' @importFrom igraph 'V<-'
#' @importFrom ggplot2 scale_size
#' @importFrom ggrepel geom_text_repel
#' @method cnetplot list
#' @export
cnetplot.list <- function(
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

    # node_label <- match.arg(node_label, c("category", "all", "none", "item", "gene", "exclusive", "share"))
    if (!node_label %in% c("category", "all", "none", "item", "gene", "exclusive", "share")) {
        if (!grepl("[><=]", node_label)) {
            stop("wrong parameter for 'node_label'")
        } else if (is.null(foldChange)) {
            stop("'foldChange' should not be NULL with the 'node_label' setting")
        }
    }

    if (node_label == "gene") node_label = "item"
    
    x <- subset_cnet_list(x, showCategory)
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

    p <- ggplot(g, layout = layout) + 
        geom_edge(color=color_edge, size=size_edge) +
        geom_point(aes(size=.data$size, alpha = I(.data$.hilight)), 
            data=td_filter(.data$.isCategory), 
            color = color_category) +
        geom_point(fc_mapping, 
            data=td_filter(!.data$.isCategory), size = 3 * size_item) +
        scale_size(range=c(3, 8) * size_category) 
    
    if (node_label == "none") {
        return(p)
    }

    if (node_label == "all") {
        p <- p + geom_text_repel(aes(label=.data$label), bg.color="white", bg.r=.1)     
        return(p)
    }

    if (node_label == "category") {
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


    p <- p + geom_text_repel(aes(label=.data$label), data = d, bg.color="white", bg.r=.1)    

    return(p)
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
        msg <- sprintf("any showCategory value that is large than %d will be removed.", n)
        message(msg)
    }
    
    showCategory <- showCategory[showCategory <= n]
    return(x[showCategory])
}

