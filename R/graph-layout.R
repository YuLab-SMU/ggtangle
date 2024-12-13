as_igraphlayout <- function(type, call = caller_env()) {
    if (type %in% igraph_layouts) {
        layout <- type
    } else {
        new_type <- sprintf("%s_%s", c('as', 'in', 'with', 'on'), type)
        i <- which(new_type %in% igraph_layouts)
        if (length(i) == 0) {
            cli::cli_abort('Cannot find the igraph layout {.val {type}}', call = call)
        }
        layout <- new_type[i]
    }

    sprintf("layout_%s", layout)
}

is_igraphlayout <- function(type) {
    if (type %in% igraph_layouts) {
        return(TRUE)
    } else {
        new_type <- sprintf("%s_%s", c('as', 'in', 'with', 'on'), type)
        if (any(new_type %in% igraph_layouts)) {
            return(TRUE)
        }
    } 
    
    return(FALSE)
}

get_igraph_layout <- function(layout) {
    if (is.function(layout)) return(layout)
    if (is_igraphlayout(layout)) {
        layout <- as_igraphlayout(layout)
    }
    get_fun_from_pkg('igraph', layout)
}


igraph_layouts <- c(
    'as_bipartite',
    'as_star',
    'as_tree',
    'in_circle',
    'nicely',
    'with_dh',
    'with_drl',
    'with_gem',
    'with_graphopt',
    'on_grid',
    'with_mds',
    'with_sugiyama',
    'on_sphere',
    'randomly',
    'with_fr',
    'with_kk',
    'with_lgl'
)

