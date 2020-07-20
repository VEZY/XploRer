
#' Get parent value
#'
#' @description Get the value of one or several variables from the parent node. This is a helper
#' function that is usually used as input to [mtg_apply()] to get the values of the parent node
#' for all nodes.
#'
#' @param ... Any node attribute (as a character)
#' @param node The node (do not put something when used from [mtg_apply()])
#' @param .scale The names of the MTG scale required (i.e. the SYMBOL from the MTG classes)
#'
#' @details This function returns the values of any attribute of the parent of a node. It is
#' mainly intended to be used in a call to [mtg_apply()] (see examples).
#'
#' @return The values of the attributes for the parent of the node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_MTG(filepath)
#' get_parent_value("Length","Width",  node = FindNode(MTG$MTG, "node_5"))
get_parent_value <- function(..., node, .scale = NULL, .type = NULL) {
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
