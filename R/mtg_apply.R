#' Apply function to tree
#'
#' @param data The input MTG, as from [read_MTG()]
#' @param fun The function to apply (see details)
#' @param scale The scale to apply the function to.
#' @param traversal The traversal order of the tree. Can be any of 'pre-order',
#' 'post-order', 'in-order', 'level', 'ancestor', or a custom function (see details)
#' @details The traversal is as in [data.tree::Traverse()]. Here's its documentation:
#'
#' The traversal order is as follows. (Note that these descriptions are not precise and complete. They are meant for quick reference only. See the data.tree vignette for a more detailed description).
#' - pre-order: Go to first child, then to its first child, etc.
#' - post-order: Go to the first branch's leaf, then to its siblings, and work your way back to the root
#' - in-order:Go to the first branch's leaf, then to its parent, and only then to the leaf's sibling
#' - level: Collect root, then level 2, then level 3, etc.
#' - ancestor: Take a node, then the node's parent, then that node's parent in turn, etc. This ignores the pruneFun
#' - function: You can also provide a function, whose sole parameter is a Node object. The function is expected to return the node's next node, a list of the node's next nodes, or NULL.
#'
#' @note The filter on scale is supposed to happen in `fun`. This approach gives the user more freedom.
#'
#' @return Nothing. The MTG is modified in place.
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_MTG(filepath)
#'
#' # Defining the function to apply, here we compute the cross-section surface:
#'
#' Width
#' fun = function(node) {
#' node$section_surface_parent = get_parent_value("Length","Width",  node = node)
#' }
#'
#' # Applying the function to the MTG:
#'   mtg_apply(MTG = MTG, fun = fun)
#'
#' # Checking it worked:
#'   mtg_df = ToDataFrameTree(mtg$MTG,"length","diameter","topological_order","section_surface","section_surface_childs","section_surface_parent")
mtg_apply = function(data,fun,scale,
                     traversal = c("pre-order", "post-order",
                                   "in-order", "level", "ancestor")){
  data$MTG$Do(fun, traversal = traversal)
}

# mutate

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
    vals = parent_value(...,node = parent, .scale = .scale)
  }
  names(vals) = dot_args
  vals
}

