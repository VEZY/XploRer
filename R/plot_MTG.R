#' Plot an MTG
#'
#' @param MTG An MTG, as from [read_MTG()]
#' @param scale The scale required for plotting
#' @param angle Insertion angle when branching
#'
#' @return A ggplot of the MTG
#' @export
#'
#' @examples
#' filepath= system.file("extdata", "simple_plant.mtg", package = "XploRer")
#' MTG= read_MTG(filepath)
#' plot(MTG)
#'
plot_MTG = function(MTG,scale= NULL,angle= 45){
  # NB: scale will be used to add information about nodes only for the nodes of the
  # scale required
  tree_df =
    ToDataFrameNetwork(MTG$MTG, "name", ".link", ".symbol", ".index")%>%
    dplyr::left_join(data.frame(SYMBOL = MTG$classes$SYMBOL, SCALE = MTG$classes$SCALE,
                                stringsAsFactors = FALSE),
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
                          angle = angle)

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

  ggplot2::ggplot(tree_df, ggplot2::aes(x = x, y = y))+
    ggplot2::geom_point()+
    ggplot2::geom_segment(ggplot2::aes(xend = x_from, yend = y_from))
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
#' rotate_point(0,0,1,0,90)
#'
#' # Example with a plot:
#' x0 = 1 ; y0 = 1 ; x1= 2 ; y1= 2 ;x3= 3 ; y3= 3
#' plot(c(x0,x1),c(y0,y1), type="l", ylim = c(0,3), xlim = c(0,3))
#' segments(x1,y1,x3,y3, col= 2)
#' segments(x1,y1,rotate_point(x1,y1,x3,y3, 45)[1],rotate_point(x1,y1,x3,y3, 45)[2], col= 3)
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
#' rotate_point(0,0,1,0,90)
extends_point = function(x0,y0,x1,y1,extend_length){
  lengthAB = sqrt((x0 - x1)^2 + (y0 - y1)^2)
  x = x1 + (x1 - x0) / lengthAB * extend_length
  y = y1 + (y1 - y0) / lengthAB * extend_length
  c(x,y)
}
