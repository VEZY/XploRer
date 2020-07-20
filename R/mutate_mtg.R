#' Mutate MTG
#'
#' @description Adds new variables to an MTG. New variables overwrite existing variables of
#'  the same name.
#'
#' @note The function was designed to be used as for [dplyr::mutate()], so it uses non-standard
#' evaluation (NSE). It also returns the `mtg` so it can be used with pipes.
#'
#' @param data A `mtg`, as returned by [read_MTG()].
#' @param ... Name-value pairs of expressions, each with length 1. To access a variable from the mtg,
#' use `node$var` instead of `var`. \
#'
#' The name of each argument will be the name of a new variable, and the value will be its corresponding value. New variables overwrite
#' existing variables of the same name. The arguments in `...` are automatically quoted and evaluated
#' in the context of the `mtg`. They support unquoting and splicing. See the chapter about
#' [metaprogramming](https://adv-r.hadley.nz/metaprogramming.html) in the book "Advanced R" from H. Wickham
#' for an introduction to these concepts.
#'
#' @return The MTG invisibly. It is returned for piping purposes only, but it is modified in-place
#'  inside de function.
#'
#' @export
#'
#' @examples
#' # Import the MTG:
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG = read_MTG(filepath)
#'
#' # And mutate it by adding two new variables, Length2 and Length3:
#' mutate_mtg(MTG, Length2 = node$Length + 2, Length3 = node$Length2 * 2)
#'
#' # note two thing here:
#' # 1/ We use "node$" to access the values of a variable inside the mtg;
#' # 2/ Length3 uses the results of Length2 before it even exist. This is because
#' # The variables are constructed sequentially.
#'
#' # We can also use pipes:
#' \dontrun{
#' read_MTG(filepath)%>%
#'   mutate_mtg(Length2 = node$Length + 2)%>%
#'   plot(.)
#' }
#'
#' mutate_mtg(MTG, Width_parent = get_parent_value("Width",  node = node))
#'
#' ToDataFrameTree(MTG$MTG,"Length","Length2","Length3")
#'
mutate_mtg = function(data,...){
  node = NULL # To avoid CRAN notes
  dots = rlang::enexprs(...)
  dots_names = names(dots)
  auto_named_dots <- names(rlang::enquos(..., .named = TRUE))

  if (length(dots) == 0L) {
    return(NULL)
  }

  dot_funs = vector(mode = "list", length = length(dots))

  for (i in seq_along(dots)) {
    not_named <- (is.null(dots_names) || dots_names[i] == "")

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
  data$MTG$Do(function(node) lapply(dot_funs, do.call, list(node = node)))
  invisible(data)
}
