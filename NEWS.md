# ggtangle 0.0.4.002

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
