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
#' children("Length", node = extract_node(MTG, "node_6"))
#'
#' # Using node 4 as reference now:
#' node_4 = extract_node(MTG, "node_4")
#' # node_4 has two children, returns two values:
#' children("Length", node = node_4)
#' # To get the names of those children:
#' children("name", node = node_4)
#'
#' # The width is not available for one child ("node_6"):
#' children("Width", node = node_4)
#'
#' # We can filter by scale if we need to return the values for some scales only:
#' children("Width", node = node_4, symbol = "Leaf")
#' # Here we get the value of node_7 also, because its parent "node_6" is not of scale
#' # "Leaf", so it was filtered out. It you need the values for one scale, but not
#' # making a recursive search from one scale to another until finding the required scale,
#' # you can put the `continue` argument to `FALSE`:
#'
#' children("Width", node = node_4, symbol = "Leaf", continue = FALSE)
#'
#' # To get the values of the children of each node:
#' mutate_mtg(MTG, children_width = children("Width"))
#' print(MTG$MTG, "Width", "children_width")
#'
#' # And using only the bodes with symbol "Leaf":
#' mutate_mtg(MTG, children_width2 = children("Width", symbol = "Leaf", continue = FALSE))
#' print(MTG$MTG, "Width", "children_width2")
children = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                               link = NULL, filter_fun = NULL,
                               continue = TRUE) {

  attribute_expr = rlang::enexpr(attribute)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'children()' is used interactively")
    }
  }

  descendants(attribute = !!attribute_expr, node = node, scale = scale, symbol = symbol,
              link = link, filter_fun = filter_fun,
              continue = continue, recursivity_level = 1)
}


#' Get leaves value
#'
#' @description Get attribute values from the terminal nodes of the descendants sub-tree of a
#' node (i.e. the "leaves" in computer science terms).
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param symbol A character vector for filtering the children by the name of their `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param scale An integer vector for filtering the `.scale` of the children (i.e. the SCALE
#'  column from the MTG classes).
#' @param link A character vector for filtering the `.link` with the children
#' @param filter_fun Any filtering function taking a node as input.
#'
#' @note This function is mainly intended to be used with [mutate_mtg()]. In this case,
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
#' # The MTG has two leaves (node_5 and node_7):
#' leaves("Length", node = MTG$MTG)
#' # We can check the function worked well by printing the symbols of each nodes:
#' print(MTG$MTG, ".symbol")
#'
#' # If we need the terminal Internodes:
#' leaves("Length", node = MTG$MTG, symbol = "Internode")
#'
#' # Or the leaves at a given scale:
#' print(MTG$MTG, ".scale")
#' leaves("Length", node = MTG$MTG, scale = 2)
#'
leaves = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                  link = NULL, filter_fun = NULL){

  attribute_expr = rlang::enexpr(attribute)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'children()' is used interactively")
    }
  }

  # Filter the leaves. The complexity here rise from the scales. The terminal node
  # at one scale can have children at another scale. So a leaf is a node that has no
  # children of the required scale/symbols/links...
  filt = rlang::new_function(rlang::pairlist2(node= , scale = scale, symbol = symbol,
                                              link = link, filter_fun = filter_fun),
                             body = quote(length(descendants("name", node = node,
                                                             scale = scale, symbol = symbol,
                                                             link = link,
                                                             filter_fun = filter_fun)) == 0))

  descendants(attribute = !!attribute_expr, node = node, scale = scale, symbol = symbol,
              link = link, filter_fun = filt,
              continue = TRUE, recursivity_level = NULL)
}
