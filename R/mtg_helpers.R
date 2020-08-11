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
#' @param recursive If a parent is not of the right `scale`, continue until the `scale`
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
#' get_parent_value(Length,  node = data.tree::FindNode(MTG$MTG, "node_5"))
#' get_parent_value("Length",  node = data.tree::FindNode(MTG$MTG, "node_5"))
get_parent_value = function(attribute, node = NULL, scale = NULL, symbol = NULL, recursive = TRUE) {

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
  is_filtered = is_scale_filtered || is_symbol_filtered

  if(node$isRoot){
    vals = NA
  }else if(!is_filtered){
    vals = parent[[attribute]]
  }else if(isTRUE(recursive)){
    vals = get_parent_value(!!attribute_expr, node = parent, scale = scale,
                            symbol = symbol)
  }else{
    vals = NA
  }

  if(is.null(vals)) vals = NA

  vals
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
#' @param recursive If a child is not of the right `scale`, continue until the `scale`
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
#' # node_5 has one child:
#' get_children_values("Length", node = data.tree::FindNode(MTG$MTG, "node_5"))
#'
#' # Using node 3 as reference now:
#' node_3 = data.tree::FindNode(MTG$MTG, "node_3")
#' # node_3 has two children, returns two values:
#' get_children_values("Length", node = node_3)
#' # To get the names of those children:
#' get_children_values("name", node = node_3)
#'
#' # The width is not available for one child ("node_5"):
#' get_children_values("Width", node = node_3)
#'
#' # We can filter by scale if we need to return the values for some scales only:
#' get_children_values("Width", node = node_3, scale = "Leaf")
#' # Here we get the value of node_6 also, because its parent "node_5" is not of scale
#' # "Leaf", so it was filtered out. It you need the values for one scale, but not
#' # making a recursive search from one scale to another until finding the required scale,
#' # you can put the `recursive` argument to `FALSE`:
#'
#' # We can also get the values recursively until finding the right value:
#' get_children_values("Width", node = node_3, scale = "Leaf", recursive = FALSE)
#'
#'
#' # To get the values of the children of each node:
#' mutate_mtg(MTG, children_width = get_children_values("Width"))
#' print(MTG$MTG, "Width", "children_width")
#'
get_children_values = function(attribute, node = NULL, scale = NULL, symbol = NULL,
                               recursive = TRUE) {

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

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

  children = node$children
  if(length(children) == 0) return(NA)

  is_children_filtered =
    unlist(lapply(children, function(x){
      # Is there any filter happening for the child node?:
      is_scale_filtered = !is.null(scale) && !x$.scale %in% scale
      is_symbol_filtered = !is.null(symbol) && !x$.symbol %in% symbol
      is_scale_filtered || is_symbol_filtered
    }))

  # Initializing the values as a vector:
  vals = vector(mode = "list",length = length(children))

  # Assigning the values read from the children:
  for (i in seq_along(children)){
    if(is_children_filtered[i]){

      if(isTRUE(recursive)){
        # If the child is not of the requested scale, try its children until
        # meeting the right scale
        vals_ = get_children_values(!!attribute_expr,node = children[[i]], scale = scale,
                                    symbol = symbol, recursive= recursive)
      }else{
        vals_ = NA
        names(vals_) = children[[i]]$name
      }
    }else{
      # Else, just return its values
      vals_ = children[[i]][[attribute]]
      if(is.null(vals_) || length(vals_) == 0) vals_ = NA
      names(vals_) = children[[i]]$name
    }

    vals[[i]] = vals_
  }

  unlist(vals)
}


#' Get ancestors value
#'
#' @description Get attribute values from all ancestors (all nodes above).
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param scale An integer vector for filtering ancestors by their `.scale` (i.e. the SCALE
#'  from the MTG classes).
#' @param symbol A character vector for filtering the names of the ancestors `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param self Return the value of the current node (`TRUE`), or the ancestors only (`FALSE`, the default)
#'
#' @details This function is mainly intended to be used with [mutate_mtg()]. In this case,
#' the `node` argument can be left empty (or you can put `node = node` equivalently).
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
#' get_ancestors_values("Length", node = data.tree::FindNode(MTG$MTG, "node_6"))
#' # Two of them have no values for Length
#'
#' # If the value of node_6 is also needed:
#' get_ancestors_values("Length", node = data.tree::FindNode(MTG$MTG, "node_6"), self = TRUE)
#'
#' # We can filter by symbol if we need to return the values for some symbols only:
#' get_ancestors_values("Width", node = data.tree::FindNode(MTG$MTG, "node_6"), symbol = "Internode")
#'
#' # The values are only returned for the ancestors with the required symbol
#' # For example we know that a leaf cannot be an ancestor because it cannot bear anything:
#' get_ancestors_values("Width", node = data.tree::FindNode(MTG$MTG, "node_6"), symbol = "Leaf")
#' # In this case it returns a length 0 vector.
#'
#' # Here we get the value of node_6 also, because its parent "node_5" is not of symbol
#' # "Leaf", so it was filtered out. It you need the values for one symbol, but not
#' # making a recursive search from one scale to another until finding the required symbol,
#' # you can put the `recursive` argument to `FALSE`:
#'
get_ancestors_values  = function(attribute, node = NULL, scale = NULL, symbol = NULL, self = FALSE){

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
      stop("node should be given when 'get_parent_value()' is used interactively")
    }
  }

  # Is there any filter happening for the current node?:
  is_scale_filtered = !is.null(scale) && !node$.scale %in% scale
  is_symbol_filtered = !is.null(symbol) && !node$.symbol %in% symbol
  is_filtered = is_scale_filtered || is_symbol_filtered


  if(isTRUE(self) && !is_filtered){
    val = node[[attribute]]
    if(!is.null(val)){
      names(val) = node$name
    }
  }else{
    val = vector()
  }

  node_current = node

  while (!data.tree::isRoot(node_current)){
    parent_val = get_parent_value(!!attribute_expr, node = node_current,
                                  scale = scale,symbol = symbol)
    node_current = node_current$parent

    # Is there any filter happening for the parent node?:
    is_scale_filtered = !is.null(scale) && !node_current$.scale %in% scale
    is_symbol_filtered = !is.null(symbol) && !node_current$.symbol %in% symbol
    is_filtered = is_scale_filtered || is_symbol_filtered

    if(is_filtered){
      next()
    }
    names(parent_val) = node_current$name
    val = c(val, parent_val)
  }

  val
}

#' Get consecutive node values
#'
#' Get the values of an attribute for all consecutive nodes (i.e. not branching)
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param symbol A character vector for filtering the names of the nodes that are considered for succession.
#' @return A vector of values named after the nodes of interest
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "tree1h.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#' get_nodes_symbol_values(attribute = "length", node = FindNode(MTG$MTG,"node_8"), symbol = "S")
#'
get_nodes_symbol_values = function(attribute, node = NULL, symbol = NULL){

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

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

  children = node$children
  if(length(children) == 0) return()

  is_children_filtered =
    unlist(lapply(children, function(x){
      # Is there any filter happening for the child node?:
      is_branching = x$.link == "+"
      is_symbol_filtered = !is.null(symbol) && !x$.symbol %in% symbol
      is_branching || is_symbol_filtered
    }))

  child = children[[which(!is_children_filtered)]]

  if(length(child) == 0){
    return()
  }else{
    vals_ = child[[attribute]]

    if(is.null(vals_) || length(vals_) == 0){
      vals_ = NA
      names(vals_) = child$name
    }

    vals_ = c(vals_,get_nodes_symbol_values(!!attribute_expr,node = child, symbol = symbol))

  }
  unlist(vals_)
}


#' Attribute as name
#'
#' This function is used to get the attribute as a character instead of an expression
#' (e.g. attribute = Length) or an extract to the node (e.g. attribute = node$Length).
#' It is used in a special context where we know the attribute will always be used to reference
#' a node (i.e. data masking).
#'
#' @param attribute a quoted expression
#'
#' @return The attribute as a character
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' attribute_as_name(rlang::expr(Length))
#' attribute_as_name(rlang::expr(node$Length))
#' attribute_as_name(rlang::expr("Length"))
#'
#' test = "Length"
#' attribute_as_name(rlang::expr(test))
#' }
attribute_as_name = function(attribute){

  if(rlang::is_call(attribute)){
    if(rlang::expr_text(attribute[2]) == "node()"){
      # Attribute given as a call (e.g. attribute = node$Length)
      attribute = gsub("\\(|\\)","",rlang::expr_text(attribute[3]))
    }else{
      stop("attribute argument should be given as attribute name (e.g. Length) or node call (e.g. node$Length)")
    }
  }else{
    # Try to execute the object to see if it exists in the environments
    # to be able to do test = "Length" ; attribute_as_name(rlang::expr(test))
    attr_obj = try(eval(attribute),silent = TRUE)

    if(!inherits(attr_obj, "try-error")){
      return(attr_obj)
    }

    # Attribute given as an expression or a character (e.g. attribute = Length, or attribute = "Length")
    attribute = rlang::as_string(attribute)

  }
  attribute
}
