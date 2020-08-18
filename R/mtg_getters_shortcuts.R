#' Get parent value
#'
#' @description Get the value of one or several variables from the parent node. This is a helper
#' function that is usually used as input to [mutate_mtg()] to get the values of the parent node
#' for all nodes.
#'
#' @param attribute Any node attribute (as a character, an expression or a node callm see details)
#' @param node The node (do not put something when used from [mutate_mtg()])
#' @param symbol A character vector for filtering the names of the MTG symbol required for the parent (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param scale An integer vector for filtering the MTG scale of the parent (i.e. the SCALE
#'  column from the MTG classes).
#' @param link A character vector for filtering the `.link` with the descendant
#' @param filter_fun Any filtering function taking a node as input, e.g. [data.tree::isLeaf()]
#'
#' @param continue If a parent is not of the right `scale`, continue until the `scale`
#' required is met if `TRUE`, or returns `NA` if `FALSE`.
#'
#' @details The `attribute` argument can be given as a string (*e.g.* attribute = "Length"), an
#' expression (*e.g.* attribute = Length) or a call to a node (*e.g.* attribute = node$Length).
#' All are equivalent because they evaluate the node given by the user or by the calling function.
#' This behavior is set to homogenize the grammar used in the calls of `mutate_mtg()`.
#'
#' @note This function returns the values of any attribute of the parent of a node. It is
#' mainly intended to be used in a call to [mutate_mtg()] (see [mutate_mtg()] doc for examples).
#'
#' @return The attribute values from the parent of a node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#' parent(Length,  node = extract_node(MTG, "node_5"))
parent = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                            link = NULL, filter_fun = NULL, continue = TRUE) {

  attribute_expr = rlang::enexpr(attribute)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'parent()' is used interactively")
    }
  }

  ancestors(!!attribute_expr, node = node, scale = scale, symbol = symbol,
                       link = link, filter_fun = filter_fun, self = FALSE,
                       continue = continue, recursivity_level = 1)
}




#' Get children value
#'
#' @description Get attribute values from the children of a node.
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param symbol A character vector for filtering the children by the name of their `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param scale An integer vector for filtering the `.scale` of the children (i.e. the SCALE
#'  column from the MTG classes).
#' @param link A character vector for filtering the `.link` with the children
#' @param filter_fun Any filtering function taking a node as input, e.g. [data.tree::isLeaf()]
#' @param continue If a child is not of the right `scale`, continue until the `scale`
#' required is met if `TRUE`, or returns `NA` if `FALSE`.
#'
#' @details This function is mainly intended to be used with [mutate_mtg()]. In this case,
#' the `node` argument can be left empty (or you can put `node = node` equivalently).
#'
#' @return The attribute values from the children of the node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#'
#' # node_6 has one child:
#' get_children_values("Length", node = extract_node(MTG, "node_6"))
#'
#' # Using node 4 as reference now:
#' node_4 = extract_node(MTG, "node_4")
#' # node_4 has two children, returns two values:
#' get_children_values("Length", node = node_4)
#' # To get the names of those children:
#' get_children_values("name", node = node_4)
#'
#' # The width is not available for one child ("node_6"):
#' get_children_values("Width", node = node_4)
#'
#' # We can filter by scale if we need to return the values for some scales only:
#' get_children_values("Width", node = node_4, symbol = "Leaf")
#' # Here we get the value of node_6 also, because its parent "node_5" is not of scale
#' # "Leaf", so it was filtered out. It you need the values for one scale, but not
#' # making a recursive search from one scale to another until finding the required scale,
#' # you can put the `continue` argument to `FALSE`:
#'
#' # We can also get the values recursively until finding the right value:
#' get_children_values("Width", node = node_3, symbol = "Leaf", continue = FALSE)
#'
#'
#' # To get the values of the children of each node:
#' mutate_mtg(MTG, children_width = get_children_values("Width"))
#' print(MTG$MTG, "Width", "children_width")
#'
get_children_values = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                               link = NULL, filter_fun = filter_fun,
                               continue = TRUE) {

  attribute_expr = rlang::enexpr(attribute)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'get_children_values()' is used interactively")
    }
  }

  descendants(!!attribute_expr, node = node, scale = scale, symbol = symbol,
              link = link, filter_fun = filter_fun,
              continue = continue, recursivity_level = 1)
}
