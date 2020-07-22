#' Get parent value
#'
#' @description Get the value of one or several variables from the parent node. This is a helper
#' function that is usually used as input to [mutate_mtg()] to get the values of the parent node
#' for all nodes.
#'
#' @param ... Any node attribute (as a character)
#' @param node The node (do not put something when used from [mutate_mtg()])
#' @param .scale The names of the MTG scale required (i.e. the SYMBOL from the MTG classes)
#'
#' @details This function returns the values of any attribute of the parent of a node. It is
#' mainly intended to be used in a call to [mutate_mtg()] (see [mutate_mtg()] doc for examples).
#'
#' @return The values of the attributes for the parent of the node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#' get_parent_value("Length","Width",  node = data.tree::FindNode(MTG$MTG, "node_5"))
get_parent_value = function(..., node, .scale = NULL) {
  parent = node$parent
  dot_args = list(...)

  if(node$isRoot){
    vals = vector(mode = "list", length = length(dot_args))
  } else if(is.null(.scale) || parent$.symbol %in% .scale){
    vals = lapply(dot_args, function(x) parent[[x]])
  }else{
    vals = get_parent_value(...,node = parent, .scale = .scale)
  }
  names(vals) = dot_args
  vals
}


#' Get children value
#'
#' @description Get attribute values from the children of a node.
#'
#' @param attribute Any node attribute (as a character)
#' @param node The node (do not put something when used from [mutate_mtg()])
#' @param .scale The names of the MTG scale required (i.e. the SYMBOL from the MTG classes)
#' @param .recursive If a child is not of the right `.scale`, continue until the `.scale`
#' required is met if `TRUE`, or returns `NA` if `FALSE`.
#'
#' @details This function returns the values of any attribute of the children of
#' a node. It is mainly intended to be used in a call to [mutate_mtg()] (see [mutate_mtg()]
#' doc for examples).
#'
#' @return The attribute values from the children of the node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#'
#' # node_5 has one child:
#' get_children_values("Length", node = data.tree::FindNode(MTG$MTG, "node_5"))
#'
#' # node_3 has two children, returns two values:
#' get_children_values("Length", node = data.tree::FindNode(MTG$MTG, "node_3"))
#' # To get the names of those children:
#' get_children_values("name", node = data.tree::FindNode(MTG$MTG, "node_3"))
#'
#' # To get the values of the children of each node:
#' mutate_mtg(MTG, children_width = get_children_values("Width", node = node))
#' print(MTG$MTG, "Width", "children_width")
#'
get_children_values = function(attribute, node, .scale = NULL, .recursive = TRUE) {
  children = node$children
  if(length(children) == 0) return(NA)
  children_in_scale =
    unlist(lapply(children, function(x){
      is.null(.scale) || x$.symbol %in% .scale
    }))

  # Initializing the values as a vector:
  vals = rep(NA, length(children))

  # Assigning the values read from the children:
  for (i in seq_along(children)){
    if(!children_in_scale[i] && .recursive){
      # If the child is not of the requested scale, try its children until
      # meeting the right scale
      vals_ = get_children_values(attribute,node = children[i], .scale = .scale,
                               .recursive= .recursive)
      if(length(vals_) > 1){
        stop("Several childs found passing a scale: expected 'follow', got 'branch'")
        # Only 'follow' is accepted after crossing a scale (it is either before or after)
      }
    }else{
      # Else, just return its values
      vals_ = children[[i]][[attribute]]
    }

    if(is.null(vals_) || length(vals_) == 0) vals_ = NA

    vals[[i]] = vals_
  }
  vals
}

