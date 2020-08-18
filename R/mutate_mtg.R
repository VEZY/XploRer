#' Mutate MTG
#'
#' @description Adds new variables to an MTG. New variables overwrite existing variables of
#'  the same name.
#'
#' @note The function was designed to be used as for [dplyr::mutate()], so it uses non-standard
#' evaluation (NSE). It also returns the `mtg` so it can be used with pipes.
#'
#' @param data A `mtg`, as returned by [read_mtg()].
#' @param ... Name-value pairs of expressions, each with length 1. To access a variable from the mtg,
#' use `node$var` instead of `var`. The name of each argument will be the name of a new variable,
#' and the value will be its corresponding value. New variables overwrite
#' existing variables of the same name. The arguments in `...` are automatically quoted and evaluated
#' in the context of the `mtg`. They support unquoting and splicing. See the chapter about
#' [metaprogramming](https://adv-r.hadley.nz/metaprogramming.html) in the book "Advanced R" from H. Wickham
#' for an introduction to these concepts.
#' @param .scale An integer vector of the `.scale` to apply the functions over (i.e. the SCALE from the MTG classes).
#' This argument is used to apply a filter on the node modification.
#' @param .symbol A character vector of the `.symbol` to apply the functions over (i.e. the SYMBOL from the MTG classes).
#' This argument is used to apply a filter on the node modification.
#' @param .traversal any of 'pre-order' (the default), 'post-order', 'in-order',
#' 'level', 'ancestor', or a custom function (see details)
#' @param .pruneFun allows providing a a prune criteria, i.e. a function taking a
#'  Node as an input, and returning `TRUE` or `FALSE`. If `pruneFun` returns `FALSE`
#'  for a Node, then the Node and its entire sub-tree will not be considered.
#'
#' @details The `.traversal` and `.pruneFun` arguments are passed to [data.tree::Traverse].
#' From its documentation: the traversal order is as follows:
#' \describe{
#'    \item{pre-order}{Go to first child, then to its first child, etc.}
#'    \item{post-order}{Go to the first branch's leaf, then to its siblings, and work your way back to the root}
#'    \item{in-order}{Go to the first branch's leaf, then to its parent, and only then to the leaf's sibling}
#'    \item{level}{Collect root, then level 2, then level 3, etc.}
#'    \item{ancestor}{Take a node, then the node's parent, then that node's parent in turn, etc. This ignores the \code{pruneFun} }
#'    \item{function}{You can also provide a function, whose sole parameter is a \code{\link{Node}} object. The
#'    function is expected to return the node's next node, a list of the node's next nodes, or NULL.}
#' }
#'
#' @seealso data.tree Do
#'
#' @return The MTG invisibly. It is returned for piping purposes only, but it is modified in-place
#'  inside de function.
#'
#' @export
#'
#' @examples
#' # Import the MTG:
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_mtg(filepath)
#'
#' # And mutate it by adding two new variables, Length2 and Length3:
#' mutate_mtg(MTG, Length2 = node$Length + 2, Length3 = node$Length2 * 2)
#'
#' # note two things here:
#' # 1/ We use "node$" to access the values of a variable inside the mtg;
#' # 2/ Length3 uses the results of Length2 before it even exist. This is because
#' # The variables are constructed sequentially.
#'
#' # We can also use pipes:
#' \dontrun{
#' read_mtg(filepath)%>%
#'   mutate_mtg(Length2 = node$Length + 2)%>%
#'   autoplot(.)
#' }
#'
#' # Or even function:
#' mutate_mtg(MTG, Length_parent = parent("Length"))
#'
#' # And more complex associations. Here is an example were we need the sum of
#' # the section_surface of children of each node:
#' mutate_mtg(MTG, section_surface = pi * ((node$Width / 2)^2),
#'            s_surf_child_sum = sum(get_children_values("section_surface"),na.rm=TRUE))
#'
#'
#' data.tree::ToDataFrameTree(MTG$MTG,"Length","Length2","Length3",
#' "Length_parent","section_surface","s_surf_child_sum")
mutate_mtg = function(data,..., .scale = NULL, .symbol = NULL,
                      .traversal = c("pre-order", "post-order",
                                     "in-order", "level", "ancestor"),
                      .pruneFun = NULL){
  node = NULL # To avoid CRAN notes
  dots = rlang::enexprs(...)
  dots_names = names(dots)
  auto_named_dots = names(rlang::enquos(..., .named = TRUE))

  if(length(dots) == 0L){
    return(NULL)
  }

  if(!is.null(.scale) && !is.null(.symbol)){
    filterFun = function(x){x$.symbol %in% .symbol && x$.scale %in% .scale}
  }else if(!is.null(.symbol)){
    filterFun = function(x){x$.symbol %in% .symbol}
  }else if(!is.null(.scale)){
    filterFun = function(x){x$.scale %in% .scale}
  }else{
    filterFun = NULL
  }

  dot_funs = vector(mode = "list", length = length(dots))

  for (i in seq_along(dots)) {
    not_named = (is.null(dots_names) || dots_names[i] == "")

    if(not_named){
      dots_names[i] = auto_named_dots[i]
    }

    if(typeof(dots[[i]]) == "language"){
      # The user needs computation for a new variable, building the code for it:
      dot_call = call("<-", substitute(node$x,list(x= make.names(dots_names[i]))), dots[[i]])
      # NB: in rlang: substitute(node$x,list(x= dots_names[i])) becomes
      # expr(`$`(node, !!dots_names[i]))

      # Building the functions:
      dot_funs[[i]] = rlang::new_function(rlang::pairlist2(node= node), body = dot_call)
    }
  }

  # Actually applying the functions to all nodes:
  data$MTG$Do(fun = function(node){lapply(dot_funs, do.call, list(node = node))},
              traversal = .traversal,
              pruneFun = .pruneFun,
              filterFun = filterFun)
  invisible(data)
}
