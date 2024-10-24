

##' Convert a list of gene IDs to igraph object.
##'
##'
##' @title Convert gene IDs to igraph object
##' @param inputList A list of gene IDs.
##' @return A igraph object.
##' @importFrom igraph graph_from_data_frame
##' @author Guangchuang Yu
##' @noRd
list2graph <- function(inputList, directed = FALSE) {
    x <- list2df(inputList)
    g <- graph_from_data_frame(x, directed=directed)
    V(g)$.isCategory <- V(g)$name %in% names(inputList)
    size <- vapply(inputList, length, FUN.VALUE=numeric(1))
    # V(g)$size <- NA # ceiling(min(size)/2)
    # V(g)$size[1:length(size)] <- size
    V(g)$size <- igraph::degree(g)
    return(g)
}

##' Convert a list of gene IDs to data.frame object.
##'
##'
##' @title Convert gene IDs to data.frame object
##' @param inputList A list of gene IDs
##' @return a data.frame object.
##' @noRd
list2df <- function(inputList) {
    # ldf <- lapply(1:length(inputList), function(i) {
    ldf <- lapply(seq_len(length(inputList)), function(i) {
        data.frame(categoryID=rep(names(inputList[i]),
                                  length(inputList[[i]])),
                   Gene=inputList[[i]])
    })

    do.call('rbind', ldf)
}

