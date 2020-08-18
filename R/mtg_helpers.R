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

#' Extract a node from the MTG
#'
#' @param mtg An mtg as from [read_mtg()]
#' @param name The name of the node, e.g. "node_1"
#'
#' @return The node (with all its information, parent, children, attributes...)
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "tree1h.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#' extract_node(MTG,"node_6")
extract_node = function(mtg,name){
  data.tree::FindNode(mtg$MTG,name)
}
