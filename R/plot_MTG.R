#' Plot an MTG
#'
#' @param MTG An MTG, as from [read_MTG()]
#' @param scale The scale required for plotting
#'
#' @return A ggplot of the MTG
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG= read_MTG(filepath)
#' plot(MTG)
#'
plot_MTG = function(MTG,scale= NULL){
  # NB: scale will be used to add information about nodes only for the nodes of the
  # scale required
  tree_df =
    ToDataFrameNetwork(MTG$MTG, "name", ".link", ".symbol", ".index")%>%
    dplyr::left_join(data.frame(SYMBOL = MTG$classes$SYMBOL, SCALE = MTG$classes$SCALE),
                     by = c(".symbol" = "SYMBOL"))%>%
    dplyr::mutate(x_inc = 0, y_inc = 0, x = 0, y = 0, x_from = 0, y_from = 0)%>%
    dplyr::mutate(x_inc = dplyr::if_else(.data$.link %in% c("+"), .data$x_inc + 1, .data$x_inc),
                  y_inc = dplyr::if_else(.data$.link %in% c("<","+"), .data$y_inc + 1, .data$y_inc))

  tree_df$from_index = match(tree_df$from, tree_df$to)

  tree_df$x[1] = 0
  tree_df$y[1] = 1
  tree_df$x_from[1] = 0
  tree_df$y_from[1] = 0

  for(i in 2:nrow(tree_df)){
    tree_df$x[i] = tree_df$x[tree_df$from_index[i]] + tree_df$x_inc[i]
    tree_df$y[i] = tree_df$y[tree_df$from_index[i]] + tree_df$y_inc[i]
    tree_df$x_from[i] = tree_df$x[tree_df$from_index[i]]
    tree_df$y_from[i] = tree_df$y[tree_df$from_index[i]]
  }

  ggplot2::ggplot(tree_df, ggplot2::aes(x = x, y = y))+
    ggplot2::geom_point()+
    ggplot2::geom_segment(ggplot2::aes(xend = x_from, yend = y_from))
}

