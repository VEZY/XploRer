#' Get ancestors value
#'
#' @description Get attribute values from all ancestors (basipetal).
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param scale Integer vector for filtering ancestors by their `.scale` (i.e. the SCALE
#'  from the MTG classes).
#' @param symbol A character vector for filtering the ancestors by their `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param link A character vector for filtering the `.link` with the parent (e.g. not a branch)
#' @param filter_fun Any filtering function taking a node as input, e.g. [data.tree::isLeaf()]
#' @param self Return the value of the current node (`TRUE`), or the ancestors only (`FALSE`, the default)
#' @param continue Boolean. If `TRUE`, the function returns all nodes that are not filtered. If `FALSE`, stop
#' at the first node that is filtered out.
#' @param recursivity_level The maximum number of recursions allowed (considering filters). E.g. to get the
#' parent only: `recursivity_level = 1`, for parent + grand-parent: `recursivity_level = 2`.
#' If `NULL` (the default), the function returns all values from the node to the root.
#'
#' @details This function is mainly intended to be used with [mutate_mtg()]. In this case,
#' the `node` argument can be left empty (or `node = node` equivalently).
#'
#' @return The attribute values from the ancestors of the node (from first parent to farther ancestor)
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#'
#' # node_6 has four ancestors:
#' get_ancestors_values("Length", node = extract_node(MTG, "node_6"))
#' # Two of them have no values for Length
#'
#' # If the value of node_6 is also needed:
#' get_ancestors_values("Length", node = extract_node(MTG, "node_6"), self = TRUE)
#'
#' # If we only need the value of the first parent:
#' get_ancestors_values("Length", node = extract_node(MTG, "node_6"), recursivity_level = 1)
#'
#' # We can filter by symbol if we need to return the values for some symbols only:
#' get_ancestors_values("Width", node = extract_node(MTG, "node_6"), symbol = "Internode")
#'
#' # The values are only returned for the ancestors with the required symbol
#' # For example we know that a leaf cannot be an ancestor because it cannot bear anything:
#' get_ancestors_values("Width", node = extract_node(MTG, "node_6"), symbol = "Leaf")
#' # In this case it returns a length 0 vector.
#'
get_ancestors_values  = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                                 link = NULL, filter_fun = NULL,self = FALSE,
                                 continue = TRUE, recursivity_level = NULL){

  if(!is.null(scale) && !is.numeric(scale)){
    stop("The scale argument must be numeric")
  }

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'get_ancestors_values()' is used interactively")
    }
  }

  # Is there any filter happening for the current node?:
  is_scale_filtered = !is.null(scale) && !node$.scale %in% scale
  is_symbol_filtered = !is.null(symbol) && !node$.symbol %in% symbol
  is_branching = !is.null(link) && !node$.link %in% link
  is_filter_fun = !is.null(filter_fun) && !filter_fun(node)

  is_filtered = is_scale_filtered || is_symbol_filtered || is_filter_fun || is_branching


  if(isTRUE(self) && !is_filtered){
    val = node[[attribute]]
    if(!is.null(val)){
      names(val) = node$name
    }
  }else{
    val = vector()
  }

  node_current = node
  level = 1 # Index of the ancestor (e.g. parent = 1, grand-parent = 2...)

  while(!data.tree::isRoot(node_current)){
    node_current = node_current$parent

    # Is there any filter happening for the parent node?:
    is_scale_filtered = !is.null(scale) && !node_current$.scale %in% scale
    is_symbol_filtered = !is.null(symbol) && !node_current$.symbol %in% symbol
    is_branching = !is.null(link) && !node_current$.link %in% link
    is_filter_fun = !is.null(filter_fun) && !filter_fun(node_current)

    is_filtered = is_scale_filtered || is_symbol_filtered || is_branching || is_filter_fun


    if(!is_filtered){
      parent_val = node_current[[attribute]]
    }else if(isTRUE(continue)){
      next()
    }else{
      # Here we stop when a parent is filtered out
      return(val)
    }

    if(is.null(parent_val)){
      parent_val = NA
    }

    names(parent_val) = node_current$name

    val = c(val, parent_val)

    if(!is.null(recursivity_level) && level == recursivity_level) return(val)

    level = level + 1
  }

  val
}



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
#' get_parent_value(Length,  node = extract_node(MTG, "node_5"))
#' get_parent_value("Length",  node = extract_node(MTG, "node_5"))
get_parent_value = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                            link = NULL, filter_fun = NULL, continue = TRUE) {

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'get_parent_value()' is used interactively")
    }
  }

  parent = node$parent

  # Is there any filter happening for the parent node?:
  is_scale_filtered = !is.null(scale) && !parent$.scale %in% scale
  is_symbol_filtered = !is.null(symbol) && !parent$.symbol %in% symbol
  is_branching = !is.null(link) && !parent$.link %in% link
  is_filter_fun = !is.null(filter_fun) && !filter_fun(parent)

  is_filtered = is_scale_filtered || is_symbol_filtered || is_filter_fun || is_branching



  if(node$isRoot){
    vals = NA
  }else if(!is_filtered){
    vals = parent[[attribute]]
  }else if(isTRUE(continue)){
    vals = get_parent_value(!!attribute_expr, node = parent, scale = scale,
                            symbol = symbol)
  }else{
    vals = NA
  }

  if(is.null(vals)) vals = NA

  vals
}
