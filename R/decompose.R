#' Get decomposing nodes
#'
#' @description Get attribute values from the nodes that are decomposing a node (e.g. the growing
#' units decomposing an axis).
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param symbol A character vector for filtering the decomposing nodes by the name of their `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param scale An integer vector for filtering the `.scale` of the decomposing nodes (i.e. the SCALE
#'  column from the MTG classes).
#' @param filter_fun Any filtering function taking a node as input.
#'
#' @details This function is mainly used to summarize attributes at a higher scale when they were measured
#' at a lower scale. For example we can think of an mtg where the length was measured at the internode scale only,
#' so this function can be used to summarize it at e.g. axis scale. The filters are used to get the nodes of interest only.
#' For example an axis can be made of internodes and leaves, but the length of an axis is made from the cumulative
#' length of the internodes only (not the leaves). A second example would be to summarize the total leaf area per axis, where
#' we would only want to use the area of the leaves (not the internodes).
#'
#' @note This function is mainly intended to be used with [mutate_mtg()]. In this case,
#' the `node` argument can be left empty (or you can put `node = node` equivalently).
#'
#' @return The attribute values from the nodes decomposing the input node
#'
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant_3.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#'
#' decompose(".symbol", node = extract_node(MTG,"node_3"), decomp_type = "symbol")
#'
#' # using filters to remove nodes that we don't need:
#' decompose(".symbol", node = extract_node(MTG,"node_3"), symbol = "Internode", decomp_type = "symbol")
#'
#' # We can check the function worked well by printing the symbols of each nodes:
#' print(MTG$MTG, ".symbol")
#'
decompose = function(attribute, node = NULL, decomp_type = c("symbol","scale"), scale = NULL,
                     symbol = NULL, link = NULL, filter_fun = NULL){

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

  # If the node is not given, use the one from the parent environment.
  # This is done to make it work from mutate_mtg without the need of
  # explicitly giving node = node as argument
  if(is.null(node)){
    if(!environmentName(env = parent.frame()) == "R_GlobalEnv"){
      node = eval(quote(node), parent.frame())
    }else{
      stop("node should be given when 'decompose()' is used interactively")
    }
  }

  decompose_(!!attribute_expr, node = node, ref_node = node, symbol = symbol, scale = scale,
             link = link, filter_fun = filter_fun, decomp_type = decomp_type)
}


#' Get decomposing nodes recursively
#'
#' This is the function actually doing the hard work for `[decompose()]`.
#'
#' @param attribute Any node attribute (as a character)
#' @param node The MTG node
#' @param ref_node The reference node to match the scale or symbol
#' @param decomp_type The type of scale to use for decomposition, either the scale or the symbol of the node.
#' @param symbol A character vector for filtering the decomposing nodes by the name of their `.symbol` (i.e. the SYMBOL
#'  column from the MTG classes).
#' @param scale An integer vector for filtering the `.scale` of the decomposing nodes (i.e. the SCALE
#'  column from the MTG classes).
#' @param link A character vector for filtering the `.link` with the descendant
#' @param filter_fun Any filtering function taking a node as input.
#'
#' @return The attributes of the nodes decomposing the reference node.
#' @keywords internal
#'
decompose_ = function(attribute, node = NULL, ref_node = NULL, scale = NULL, symbol = NULL, link = NULL,
                      filter_fun = NULL,
                      decomp_type = c("scale","symbol")){

  attribute_expr = rlang::enexpr(attribute)
  attribute = attribute_as_name(attribute_expr)

  children = node$children
  if(length(children) == 0) return(NULL)

  # Are the children filtered?
  is_children_filtered =
    unlist(lapply(children, function(x){
      # Is there any filter happening for the child node?:
      is_branching = !is.null(link) && !x$.link %in% link
      is_symbol_filtered = !is.null(symbol) && !x$.symbol %in% symbol
      is_scale_filtered = !is.null(scale) && !x$.scale %in% scale
      is_filter_fun = !is.null(filter_fun) && !filter_fun(x)

      is_branching || is_symbol_filtered || is_scale_filtered || is_filter_fun
    }))

  # Is the children of the same scale (or symbol) than the node?
  is_children_ref =
    unlist(lapply(children, function(x){
      if(decomp_type == "scale"){
        x$.scale == ref_node$.scale
      }else{
        x$.symbol == ref_node$.symbol
      }
    }))

  if(all(is_children_ref)){
    return(NULL)
  }

  # Prune the tree if any children is of the same scale (or symbol) than the node
  children = children[which(!is_children_ref)]
  is_children_ref = is_children_ref[which(!is_children_ref)]
  is_children_filtered = is_children_filtered[which(!is_children_ref)]

  vals_ = unlist(lapply(children, function(x){
    x = x[[attribute]]
    if(is.null(x)){x = NA}
    x
  }))

  if(is.null(vals_) || length(vals_) == 0){
    vals_ = rep(NA, length(children))
    names(vals_) = unlist(lapply(children, function(x){x$name}))
  }

  # Keep all children but keep the values only for the ones not filtered
  vals_ = vals_[!is_children_filtered]

  vals_children =
    lapply(children, function(x,recurs){
      decompose_(!!attribute_expr, node = x, ref_node = ref_node, symbol = symbol, scale = scale,
                 link = link, filter_fun = filter_fun, decomp_type = decomp_type)
    })

  vals_ = c(vals_, unlist(vals_children))

  names(vals_) = stringr::str_extract(string = names(vals_),
                                      pattern = "node_[[:digit:]]+$")
  unlist(vals_)
}
