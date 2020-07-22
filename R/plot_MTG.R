#' Plot an MTG
#'
#' @param MTG An MTG, as from [read_mtg()]
#' @param scale The scale required for plotting
#' @param angle Insertion angle when branching
#' @param phylotaxy Is phylotaxy required ? Uses 180 degrees if `TRUE`.
#' @param ... Further arguments. [plotly_mtg()] uses this to add variables to the output data.
#'
#' @details The function needs the topological orders as attributes in the MTG. If they
#' are not present, it uses [topological_order()] to compute it with descending order.
#' If you need ascending order, please use `topological_order(MTG, ascend = TRUE)` before
#' plotting.
#'
#' @return A ggplot of the MTG
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(ggplot2)
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG= read_mtg(filepath)
#' autoplot(MTG)
autoplot.mtg = function(MTG, scale = NULL, angle = 45, phylotaxy = TRUE,...){
  # NB: scale will be used to add information about nodes only for the nodes of the
  # scale required

  if(!inherits(MTG,"mtg")){
    stop("The ... arguments should start with an MTG as returned by read_mtg()")
  }

  dots = rlang::enexprs(...)
  dot_names = names(dots)
  auto_named_dots = names(rlang::enquos(..., .named = TRUE))
  not_named = unlist(lapply(dot_names, function(x) (is.null(x) || x == "")))
  dot_names[not_named] = auto_named_dots[not_named]
  names(dots) = dot_names

  # Compute the topological order if missing from the MTG:
  if(!"topological_order" %in% MTG$MTG$fieldsAll){
    topological_order(MTG)
  }

  if(length(dots) == 0){
    tree_df =
      data.tree::ToDataFrameNetwork(MTG$MTG, "name", ".link", ".symbol", ".index",
                                    "topological_order")
  }else{
    name_list = as.character(dots)
    names(name_list) = dot_names
    tree_df =
      data.tree::ToDataFrameNetwork(MTG$MTG, "name", ".link", ".symbol", ".index",
                                    "topological_order",as.character(dots))%>%
      dplyr::rename(!!!name_list)

    # NB : all these tricks because ggplotly outputs the name of the variable in the data
    # for the tooltip instead of the name of the mapping... So what we do here is to rename
    # the column in the data and the mapping to match the names given by the user...
    dots =
      mapply(function(x,y){
        x = rlang::sym(y)
      },dots,dot_names)
  }

  tree_df =
    tree_df%>%
    dplyr::left_join(data.frame(SYMBOL = MTG$classes$SYMBOL, SCALE = MTG$classes$SCALE,
                                stringsAsFactors = FALSE),
                     by = c(".symbol" = "SYMBOL"))%>%
    dplyr::group_by(.data$topological_order)%>%
    dplyr::mutate(phylotaxy = seq_along(topological_order) %% 2)
  # phylotaxy is first computed as a sequence along the topology, and then it is
  # set to 0 for each even number, and to 1 for uneven ones.It allows to altern the
  # phylotaxy along the axis.

  tree_df$phylotaxy[tree_df$phylotaxy == 0] = -1 # all 0 replaced by -1 (either 1 or -1 now)
  tree_df$from_index = match(tree_df$from, tree_df$to)

  tree_df$x = 0
  tree_df$y = 0
  tree_df$x_from = 0
  tree_df$y_from = 0
  tree_df$y[1] = 1

  for(i in 2:nrow(tree_df)){
    if(tree_df$.link[i] == "+"){

      point= extends_point(x0 = tree_df$x_from[tree_df$from_index[i]],
                           y0 = tree_df$y_from[tree_df$from_index[i]],
                           x1 = tree_df$x[tree_df$from_index[i]],
                           y1 = tree_df$y[tree_df$from_index[i]],
                           extend_length = 1)

      point= rotate_point(x0 = tree_df$x[tree_df$from_index[i]],
                          y0 = tree_df$y[tree_df$from_index[i]],
                          x1 = point[1],
                          y1 = point[2],
                          angle = tree_df$phylotaxy[i] * angle)

    }else if(tree_df$.link[i] == "<"){
      point= extends_point(x0 = tree_df$x_from[tree_df$from_index[i]],
                           y0 = tree_df$y_from[tree_df$from_index[i]],
                           x1 = tree_df$x[tree_df$from_index[i]],
                           y1 = tree_df$y[tree_df$from_index[i]],
                           extend_length = 1)
    }else{
      # Here we are in the case of decomposing, no increment in the representation.
      point= extends_point(x0 = tree_df$x_from[tree_df$from_index[i]],
                           y0 = tree_df$y_from[tree_df$from_index[i]],
                           x1 = tree_df$x[tree_df$from_index[i]],
                           y1 = tree_df$y[tree_df$from_index[i]],
                           extend_length = 0.001)
    }
    tree_df$x[i] = point[1]
    tree_df$y[i] = point[2]

    # Saving the coordinates of the parent for segments:
    tree_df$x_from[i] = tree_df$x[tree_df$from_index[i]]
    tree_df$y_from[i] = tree_df$y[tree_df$from_index[i]]
  }

  tree_df%>%
    dplyr::ungroup()%>%
    dplyr::mutate(topological_order = as.factor(.data$topological_order))%>%
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y,
                                 color = .data$topological_order,
                                 name = .data$name,
                                 link= .data$.link,
                                 symbol = .data$.symbol,
                                 index = .data$.index,
                                 !!!dots))+
    ggplot2::geom_point()+
    ggplot2::geom_segment(ggplot2::aes(xend = .data$x_from, yend = .data$y_from))+
    ggplot2::labs(color = "Topological order")
}

#' Plot an interactive MTG
#'
#' @param MTG An MTG, as from [read_mtg()]
#' @param ... Names of the variables to be added to the tooltip (see details and examples).
#' @param .scale The scale required for plotting
#' @param .angle Insertion angle when branching
#' @param .phylotaxy Is phylotaxy required ? Uses 180 degrees if `TRUE`.
#'
#' @details The name of each argument in `...` will be the name of a the variable given in the tooltip,
#' and the value will be the value of the corresponding variable given as value. The arguments in `...` are
#' automatically quoted and evaluated in the context of the `mtg`. They support unquoting and splicing. See
#' the chapter about [metaprogramming](https://adv-r.hadley.nz/metaprogramming.html) in the book "Advanced R"
#' from H. Wickham for an introduction to these concepts.
#'
#' @seealso [mutate_mtg()] to compute variables and add it to the tooltip with `...`.
#' @return A [plotly] object of the MTG
#' @export
#'
#' @importFrom ggplot2 autoplot
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG= read_mtg(filepath)
#' plotly_mtg(MTG)
#'
#' # We can add more information to the tooltip, e.g. the values of the Length column:
#' plotly_mtg(MTG, Length)
#'
#' # We can also use custom names for the variable in the tooltip:
#' plotly_mtg(MTG, node_width = Width)
#' # Here the tooltip will show the Width, labeled as "node_width"
#'
plotly_mtg = function(MTG, ..., .scale = NULL, .angle = 45, .phylotaxy = TRUE){
  mtg_plot = autoplot.mtg(MTG, scale = .scale, angle = .angle, phylotaxy = .phylotaxy,...)

  dots = rlang::enexprs(...)
  dot_names = names(dots)
  auto_named_dots = names(rlang::enquos(..., .named = TRUE))
  not_named = unlist(lapply(dot_names, function(x) (is.null(x) || x == "")))
  dot_names[not_named] = auto_named_dots[not_named]

  plotly::ggplotly(p = mtg_plot, tooltip= c("name", ".link", ".symbol", ".index",dot_names))
}

#' Rotate coordinates
#'
#' @param x0 x coordinate of the origin
#' @param y0 y coordinate of the origin
#' @param x1 x coordinate of the point
#' @param y1 y coordinate of the point
#' @param angle Rotation angle in degree (anti-clockwise)
#'
#' @return The new coordinates for the point
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' rotate_point(0,0,1,0,90)
#'
#' # Example with a plot:
#' x0 = 1 ; y0 = 1 ; x1= 2 ; y1= 2 ;x3= 3 ; y3= 3
#' plot(c(x0,x1),c(y0,y1), type="l", ylim = c(0,3), xlim = c(0,3))
#' segments(x1,y1,x3,y3, col= 2)
#' segments(x1,y1,rotate_point(x1,y1,x3,y3, 45)[1],rotate_point(x1,y1,x3,y3, 45)[2], col= 3)
#' }
rotate_point = function(x0,y0,x1,y1,angle){
    angle = - angle * pi / 180
    x1 = x1 - x0
    y1 = y1 - y0
    cos_a = cos(angle)
    sin_a = sin(angle)

    x = x1 * cos_a - y1 * sin_a + x0
    y = x1 * sin_a + y1 * cos_a + y0
    c(x,y)
}

#' Extends coordinate
#'
#' @param x0 x coordinate of the origin
#' @param y0 y coordinate of the origin
#' @param x1 x coordinate of the point
#' @param y1 y coordinate of the point
#' @param extend_length ROtation angle in degree (anti-clockwise)
#'
#' @return The new coordinates for the point
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' extends_point(0,0,1,0,90)
#' }
extends_point = function(x0,y0,x1,y1,extend_length){
  lengthAB = sqrt((x0 - x1)^2 + (y0 - y1)^2)
  x = x1 + (x1 - x0) / lengthAB * extend_length
  y = y1 + (y1 - y0) / lengthAB * extend_length
  c(x,y)
}
