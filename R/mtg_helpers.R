#' Get parent value
#'
#' @description Get the value of one or several variables from the parent node. This is a helper
#' function that is usually used as input to [mutate_mtg()] to get the values of the parent node
#' for all nodes.
#'
#' @param attribute Any node attribute (as a character)
#' @param node The node (do not put something when used from [mutate_mtg()])
#' @param scale The names of the MTG scale required for the parent (i.e. the SYMBOL
#'  from the MTG classes). Used as a filter.
#' @param recursive If a parent is not of the right `scale`, continue until the `scale`
#' required is met if `TRUE`, or returns `NA` if `FALSE`.
#'
#' @details This function returns the values of any attribute of the parent of a node. It is
#' mainly intended to be used in a call to [mutate_mtg()] (see [mutate_mtg()] doc for examples).
#'
#' @return The attribute values from the parent of a node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#' get_parent_value("Length",  node = data.tree::FindNode(MTG$MTG, "node_5"))
get_parent_value = function(attribute, node = NULL, scale = NULL, recursive = TRUE) {

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

  if(node$isRoot){
    vals = NA
  } else if(is.null(scale) || parent$.symbol %in% scale){
    vals = parent[[attribute]]
  }else if(isTRUE(recursive)){
    vals = get_parent_value(attribute, node = parent, scale = scale)
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
#' @param scale The names of the MTG scale(s) required for the children (i.e. the SYMBOL
#'  from the MTG classes). Used as a filter.
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
get_children_values = function(attribute, node = NULL, scale = NULL, recursive = TRUE) {

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
  children_in_scale =
    unlist(lapply(children, function(x){
      is.null(scale) || x$.symbol %in% scale
    }))

  # Initializing the values as a vector:
  vals = vector(mode = "list",length = length(children))

  # Assigning the values read from the children:
  for (i in seq_along(children)){
    if(!children_in_scale[i]){

      if(isTRUE(recursive)){
        # If the child is not of the requested scale, try its children until
        # meeting the right scale
        vals_ = get_children_values(attribute,node = children[[i]], scale = scale,
                                    recursive= recursive)
      }else{
        vals_ = NA
      }
    }else{
      # Else, just return its values
      vals_ = children[[i]][[attribute]]
      if(!is.null(vals_)){
        names(vals_) = children[[i]]$name
      }
    }

    if(is.null(vals_) || length(vals_) == 0) vals_ = NA

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
#' @param scale Filter ancestors by the names of the MTG scale(s) (i.e. the SYMBOL
#'  from the MTG classes).
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
#' # We can filter by scale if we need to return the values for some scales only:
#' get_ancestors_values("Width", node = data.tree::FindNode(MTG$MTG, "node_6"), scale = "Internode")
#'
#' # The values are only returned for the ancestors with the required scale.
#' # For example we know that a leaf cannot be an ancestor because it cannot bear anything:
#' get_ancestors_values("Width", node = data.tree::FindNode(MTG$MTG, "node_6"), scale = "Leaf")
#' # In this case it returns a length 0 vector.
#'
#' # Here we get the value of node_6 also, because its parent "node_5" is not of scale
#' # "Leaf", so it was filtered out. It you need the values for one scale, but not
#' # making a recursive search from one scale to another until finding the required scale,
#' # you can put the `recursive` argument to `FALSE`:
#'
get_ancestors_values  = function(attribute, node = NULL, scale = NULL, self = FALSE){

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

  if(isTRUE(self) && (is.null(scale) || node$.symbol %in% scale)){
    val = node[[attribute]]
    if(!is.null(val)){
      names(val) = node$name
    }
  }else{
    val = vector()
  }

  node_current = node

  while (!data.tree::isRoot(node_current)){
    parent_val = get_parent_value(attribute, node = node_current, scale = scale)
    node_current = node_current$parent
    if(!is.null(scale) && !node_current$.symbol %in% scale){
      next()
    }
    names(parent_val) = node_current$name
    val = c(val, parent_val)
  }

  val
}

