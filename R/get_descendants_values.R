#' Get descendants values
#'
#' @description Get attribute values from the descendants (acropetal).
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param scale An integer vector for filtering descendant by their `.scale` (i.e. the SCALE
#'  from the MTG classes).
#' @param symbol A character vector for filtering the names of the descendant `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param link A character vector for filtering the `.link` with the descendant
#' @param continue Boolean. If `TRUE`, the function returns all nodes that are not filtered. If `FALSE`, stop
#' at the first node that is filtered out.
#' @param self Return the value of the current node (`TRUE`), or the ancestors only (`FALSE`, the default)
#' @param filter_fun Any filtering function taking a node as input, e.g. [data.tree::isLeaf()]
#' @param recursivity_level The maximum number of recursions allowed (considering filters). E.g. to get only the
#' children, `recursivity_level = 1`, if children + their children: `recursivity_level = 2`.
#' If `NULL` (the default), the function returns all values from the node to the root.
#'
#' @details This function is mainly intended to be used with [mutate_mtg()]. In this case,
#' the `node` argument can be left empty (or you can put `node = node` equivalently).
#'
#' @return The attribute values from the descendant(s) of the node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "tree1h.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#' node_8 = extract_node(MTG,"node_8")
#' # getting all descendants of node_8
#' descendants(attribute = "length", node = node_8)
#'
#' # getting all descendants of node_8, but only the nodes with symbol "S":
#' descendants(attribute = "length", node = node_8, symbol = "S")
#'
#' # getting all descendants of node_8, but only the nodes with symbol "S", and not
#' # recursively, i.e. we stop the search for a child if it is filtered out (we don't
#' # go to its own children)
#' descendants(attribute = "length", node = node_8, symbol = "S",
#'                        continue = FALSE)
#'
#' # getting the children of node_8 (and not below):
#' descendants(attribute = "length", node = node_8, recursivity_level = 1)
#' # getting the children of node_8 and their children:
#' descendants(attribute = "length", node = node_8, recursivity_level = 2)
#' # getting the children of node_8 and their children, and filter for "S":
#' descendants(attribute = "length", node = node_8, symbol = "S", recursivity_level = 2)
#' # The function returns until node_12 because node_10 is not an "S" and is then filtered out
#' # which makes node_12 two levels below node.
#'
#' # To get the descendants of a node but only for the nodes following it, not
#' # branching (e.g. for an axis):
#' descendants(attribute = "length", node = node_8, symbol = "S",
#'                        link = c("/","<"), continue = FALSE)
#'
#' # To get the values for the leaves (i.e. the last node) only:
#' descendants(attribute = "length", node = node_8, filter_fun = data.tree::isLeaf)
#'
#' # Length were observed at the "S" scale (S = segment of an axis between two branches),
#' # but we need the length at the axis scale, to do so:
#' mutate_mtg(MTG,
#'            axis_length = sum(descendants(attribute = "length", symbol = "S",
#'                                                    link = c("/","<"), continue = FALSE)),
#'            .symbol = "A")
descendants = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                                  link = NULL, continue = TRUE, self = FALSE,
                                  filter_fun = NULL, recursivity_level = NULL){

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'descendants()' is used interactively")
    }
  }

  # Is there any filter happening for the current node?:
  is_branching = !is.null(link) && !node$.link %in% link
  is_symbol_filtered = !is.null(symbol) && !node$.symbol %in% symbol
  is_scale_filtered = !is.null(scale) && !node$.scale %in% scale
  is_filter_fun = !is.null(filter_fun) && !filter_fun(node)

  is_filtered = is_branching || is_symbol_filtered || is_scale_filtered || is_filter_fun

  if(isTRUE(self) && !is_filtered){
    val = node[[attribute]]
    if(!is.null(val)){
      names(val) = node$name
    }
  }else{
    val = NULL
  }

  if(!is.null(recursivity_level) && recursivity_level == 0) return(val)

  children = node$children
  if(length(children) == 0) return(val)

  is_children_filtered =
    unlist(lapply(children, function(x){
      # Is there any filter happening for the child node?:
      is_branching = !is.null(link) && !x$.link %in% link
      is_symbol_filtered = !is.null(symbol) && !x$.symbol %in% symbol
      is_scale_filtered = !is.null(scale) && !x$.scale %in% scale
      is_filter_fun = !is.null(filter_fun) && !filter_fun(x)

      is_branching || is_symbol_filtered || is_scale_filtered || is_filter_fun
    }))

  if(all(is_children_filtered) && isFALSE(continue)){
    return(val)
  }

  if(isFALSE(continue)){
    # If not recursive, prune the tree where filtered
    children = children[which(!is_children_filtered)]
  }

  if(length(children) == 0){
    return(val)
  }

  vals_ = unlist(lapply(children, function(x){
    x = x[[attribute]]
    if(is.null(x)){x = NA}
    x
  }))
  # names(vals_) = unlist(lapply(children, function(x){x$name}))

  if(is.null(vals_) || length(vals_) == 0){
    vals_ = rep(NA, length(children))
    names(vals_) = unlist(lapply(children, function(x){x$name}))
  }

  if(isTRUE(continue)){
    # If recursive, keep all children but keep the values only for the ones not filtered
    vals_ = vals_[!is_children_filtered]
  }

  if(!is.null(recursivity_level)){
    # Decreasing recursivity_level by one for the children not filtered. And keeping
    # it to its value for the children that are filtered out to be able to continue below
    # if needed:
    recursivity_level = rep(recursivity_level,length(children))
    recursivity_level[!is_children_filtered] =
      recursivity_level[!is_children_filtered] - 1

    vals_children =
      mapply(function(x,recurs){
        descendants(!!attribute_expr, node = x, symbol = symbol, scale = scale,
                    link = link, continue = continue, self = FALSE,
                    filter_fun = filter_fun,
                    recursivity_level = recurs)
      }, children, recurs = recursivity_level, SIMPLIFY = FALSE)
  }else{
    vals_children =
      lapply(children, function(x,recurs){
        descendants(!!attribute_expr, node = x, symbol = symbol, link = link,
                               continue = continue, self = FALSE,
                               filter_fun = filter_fun,
                               recursivity_level = recursivity_level)
      })
  }

  vals_ = c(val, vals_, unlist(vals_children))

  names(vals_) = stringr::str_extract(string = names(vals_),
                                      pattern = "node_[[:digit:]]+$")
  unlist(vals_)
}

