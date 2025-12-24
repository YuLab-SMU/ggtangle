#' Glycan Layout Algorithm (Fishbone/SNFG-like)
#'
#' Calculates node coordinates for a glycan structure to mimic SNFG style.
#' Supports customizable direction and branch length.
#'
#' @param graph An igraph object.
#' @param direction The direction of the main chain growth ("left", "right", "up", "down"). Default is "left".
#' @param length The distance between nodes. Default is 1.
#' @param angle_sep The angle separation for branches in degrees. Default is 30.
#' @param ... Additional arguments.
#' @return A matrix of x, y coordinates.
#' @importFrom igraph V neighbors degree
#' @export
layout_fishbone <- function(graph, direction = "left", length = 1, angle_sep = 30, ...) {
  # Ensure we have a graph
  if (!inherits(graph, "igraph")) stop("Input must be an igraph object")
  
  # Initialize coordinates
  coords <- matrix(0, nrow = length(igraph::V(graph)), ncol = 2)
  rownames(coords) <- as.character(igraph::V(graph))
  colnames(coords) <- c("x", "y")
  
  # 1. Identify Root
  # In our parser (Child -> Parent), Root is the node with out-degree 0.
  out_deg <- igraph::degree(graph, mode = "out")
  root_id <- which(out_deg == 0)
  
  # If multiple roots (forest), take the first one
  if (length(root_id) == 0) root_id <- 1 # Fallback
  root_id <- root_id[1]
  
  # Helper to get base angle from direction string
  get_base_angle <- function(dir) {
     if (dir == "left")  return(180)
     if (dir == "right") return(0)
     if (dir == "up")    return(90)
     if (dir == "down")  return(270)
     return(180) # Default
  }
  
  # Helper to get subtree size (for main chain detection)
  get_subtree_size <- function(node) {
    children <- igraph::neighbors(graph, node, mode = "in")
    if (length(children) == 0) return(1)
    sum(sapply(children, get_subtree_size)) + 1
  }
  
  # Start layout
  base_angle <- get_base_angle(direction)
  
  # Recursive layout function using vector angles
  layout_node <- function(node, x, y, current_angle) {
    # Set coord
    coords[node, 1] <<- x
    coords[node, 2] <<- y
    
    # Get children (incoming edges)
    children <- igraph::neighbors(graph, node, mode = "in")
    if (length(children) == 0) return()
    
    # Get linkages for children
    child_data <- data.frame(id = as.numeric(children), size = 0, linkage = "", stringsAsFactors = FALSE)
    
    for (i in 1:length(children)) {
      child <- children[i]
      child_data$size[i] <- get_subtree_size(child)
      
      # Find edge
      e_id <- igraph::get_edge_ids(graph, c(as.numeric(child), as.numeric(node)))
      linkage <- igraph::edge_attr(graph, "label", index = e_id)
      child_data$linkage[i] <- ifelse(is.null(linkage), "", linkage)
    }
    
    # Sort children: Main chain preference (largest subtree first)
    # BUT for symmetric split, we might want consistent ordering (e.g. 6 top, 3 bottom)
    # Let's add a sort key: 6 -> 10, 4 -> 5, 2 -> 2, 3 -> 0
    child_data$sort_key <- 5 # Default middle
    child_data$sort_key[grepl("6", child_data$linkage)] <- 10
    child_data$sort_key[grepl("4", child_data$linkage)] <- 5
    child_data$sort_key[grepl("2", child_data$linkage)] <- 2
    child_data$sort_key[grepl("3", child_data$linkage)] <- 0
    
    # Sort by sort_key descending (6 top, 3 bottom), then by size
    child_data <- child_data[order(child_data$sort_key, child_data$size, decreasing = TRUE), ]
    
    n_children <- nrow(child_data)
    
    # Calculate angles
    # Rule 1: No bifurcation (1 child) -> Straighten to Main Axis (Horizontal/Vertical)
    # Rule 2: Bifurcation (>=2 children) -> Symmetric split relative to Current Angle
    
    if (n_children == 1) {
       # Single child: Snap to base_angle (Horizontal/Vertical)
       # User request: "If no further branching, keep horizontal or vertical."
       # This means we ignore the current_angle (which might be tilted from a previous split)
       # and reset to the global base_angle.
       
       # Exception: If we want to support "L" turns for specific linkages (e.g. 1-6 vertical),
       # we could do it here. But "Horizontal OR Vertical" usually implies main axis alignment for tails.
       # Let's use base_angle.
       
       # Calculate dev needed to reach base_angle
       # abs_angle = base_angle
       # dev = base_angle - current_angle
       # We handle abs_angle directly below.
       
       child_data$abs_angle <- base_angle
       
    } else {
       # Bifurcation: Split relative to Current Angle (Flow)
       # Symmetric split logic
       
       # Determine orientation:
       # If moving Left (180 deg), "Up" is Clockwise (-).
       # If moving Right (0 deg), "Up" is Counter-Clockwise (+).
       # If moving Up (90 deg), "Left" is CCW (+).
       # If moving Down (270 deg), "Right" is CCW (+).
       
       # We want 6 to be "Up/Left" (Top side visually).
       # If cos(angle) < -0.1 (Leftish) -> Flip sign (make 6 negative dev)
       # Else -> Keep sign (make 6 positive dev)
       
       dev_sign <- if (cos(current_angle * pi / 180) < -0.1) -1 else 1
       
       if (n_children == 2) {
          factors <- c(1, -1)
       } else {
          # General case: spread by 1 unit
          mid <- (n_children + 1) / 2
          factors <- (mid - 1:n_children) 
       }
       
       # Apply sign flip so factors[1] (Linkage 6) goes to Visual Top
       child_data$dev <- factors * angle_sep * dev_sign
       child_data$abs_angle <- current_angle + child_data$dev
    }

    for (i in 1:nrow(child_data)) {
      child_id <- child_data$id[i]
      lnk <- child_data$linkage[i]
      abs_angle <- child_data$abs_angle[i]
      
      # Calculate Position
      # Convert angle to radians
      rad <- abs_angle * pi / 180
      
      # New coords
      nx <- x + length * cos(rad)
      ny <- y + length * sin(rad)
      
      # Recurse
      # Pass the new angle as the reference for the child's children
      layout_node(child_id, nx, ny, abs_angle)
    }
  }
  
  layout_node(root_id, 0, 0, base_angle)
  
  return(coords)
}
