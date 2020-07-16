#' Topological order
#'
#' @description Compute the topological order of a tree
#'
#' @param MTG An MTG tree as from [read_MTG()]
#'
#' @return Nothing, update the values of the MTG in-place. The MTG is enriched with the
#'  topological order of each node
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG= read_MTG(filepath)
#' topological_order(MTG)
#' print(MTG$MTG, "topological_order")
topological_order = function(MTG){
  MTG$MTG$Do(
    function(node){
      parent_order = node$parent$topological_order
      if(is.null(parent_order)){
        parent_order = 1
      }
      if (node$.link == "+") {
        node$topological_order = parent_order + 1
      } else {
        node$topological_order = parent_order
      }
    })
}
