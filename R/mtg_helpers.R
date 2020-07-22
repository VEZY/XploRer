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


#' Get parent value
#'
#' @description Get the value of one or several variables from the parent node. This is a helper
#' function that is usually used as input to [mutate_mtg()] to get the values of the parent node
#' for all nodes.
#'
#' @param ... Any node attribute (as a character)
#' @param node The node (do not put something when used from [mutate_mtg()])
#' @param .scale The names of the MTG scale required (i.e. the SYMBOL from the MTG classes)
#' @param .recursive If a child is not of the right `.scale`, continue until the `.scale`
#' required is met if `TRUE`, or returns `NA` if `FALSE`.
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
#'
#' children = MTG$MTG$node_2$node_3$children
get_child_values = function(..., node, .scale = NULL, .recursive = TRUE) {
  children = node$children
  dot_args = list(...)

  children_in_scale =
    unlist(lapply(children, function(x){
      is.null(.scale) || x$.symbol %in% .scale
    }))

  # Initializing the values list:
  vals = lapply(dot_args, function(x){rep(NA, length(children))})
  names(vals) = unlist(dot_args)
  # NB: important to initialize as growing objects is bad practice in R.
  # microbenchmarking shows it is 30% longer without init.

  # Assigning the values read from the childs:

  for (j in seq_along(children)){
    if(!children_in_scale[j] && .recursive){
      # If the child is not of the requested scale, try its children until
      # meeting the right scale
      vals_ = get_child_values(...,node = children[i], .scale = .scale,
                               .recursive= .recursive)
      for(i in seq_along(dot_args)){
        # Else, just return its values
        vals[[i]][[j]] = vals_
      }
    }else{
      for(i in seq_along(dot_args)){
        # Else, just return its values
        vals[[i]][j] = children[[j]][[dot_args[[i]]]]
      }
    }
  }
  vals
}
