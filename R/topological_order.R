#' Topological order
#'
#' @description Compute the topological order of a tree
#'
#' @param MTG An MTG tree as from [read_mtg()]
#' @param ascend Is the order computed from the base (`TRUE`), or the tip (`FALSE`) ?
#'
#' @return Nothing, update the values of the MTG in-place. The MTG is enriched with the
#'  topological order of each node
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG= read_mtg(filepath)
#' topological_order(MTG)
#' print(MTG$MTG, "topological_order")
topological_order = function(MTG, ascend = FALSE){
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

  if(!ascend){
    # Reverse the order of the topology orders
    orders = MTG$MTG$Get("topological_order")
    descend_orders = rep(NA_integer_, length(orders))

    ascend_orders = unique(orders)
    rev_orders = rev(ascend_orders)

    for(i in seq_along(ascend_orders)){
      descend_orders[orders == ascend_orders[i]] = rev_orders[i]
    }

    MTG$MTG$Set(topological_order = descend_orders)
  }
}
