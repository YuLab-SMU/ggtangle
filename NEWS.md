# ggtangle 0.0.6.001

+ add dependency on R >=4.1.0 as `|>` was used (2025-04-30, Wed)

# ggtangle 0.0.6

+ update DESCRIPTION to add citation (2024-12-17, Tue)
  - G Yu (2018) <doi:10.1093/molbev/msy194>
+ allows using layout name (e.g., 'nicely', 'kk') as in 'ggraph' (2024-12-13, Fri)

# ggtangle 0.0.5

+ allows `color_edge = "category"` to color edges based on the category information in `cnetplot()` (2024-11-29, Fri)
+ update `cnetplot()` to address issues (2024-11-15, Fri)
    - <https://github.com/YuLab-SMU/enrichplot/issues/293#issuecomment-2462502566>
+ update vignette to add a session of `cnetplot()` (2024-11-06, Wed)
+ 'node_label' can be a vector of selected items/genes to specify the items displayed in `cnetplot()` (2024-11-06, Wed)
    - if `options(cnetplot_subset = TRUE)`, all the edges and nodes of the genes that are not wanted to labeled will be remove 
+ `geom_cnet_label()` allows detail adjustment of `cnetplot()` labels (2024-11-06, Wed)
    - <https://github.com/YuLab-SMU/enrichplot/issues/194>
+ 'node_label' in `cnetplot()` supports new arguments (2024-11-05, Tue)
    - 'exclusive' to label genes that is uniquely belong to categories
    - 'share' to label genes that are shared between categories
    - expresson, like '> 1' or '< 1', to label those genes with `foldChange > 1` or `foldChange < 1`
    - <https://github.com/YuLab-SMU/enrichplot/issues/253>

# ggtangle 0.0.4

+ compatible with more igraph layouts (2024-10-29, Tue)

# ggtangle 0.0.3

+ `drag_network()` from 'enrichplot' (2024-10-24, Thu)
+ `cnetplot()` method for 'list' (2024-10-24, Thu)
+ re-export `ggfun::td_filter()` and `ggplot2::geom_point()`

# ggtangle 0.0.2

+ `geom_edge()` layer to draw network (2024-08-28, Wed) 
+ extend `ggplot()` to support 'igraph' object (2024-08-28, Wed) 
