#' Get area of convex hull
#'
#' @param data points to find a convex hull over (assumed n x 2)
#' @param c_position Position of the longitude/latitude pair
#'
#' @return 
#' \item{polygon}{dataframe}
#' \item{size}{area of the convex hull}
#' \item{poly}{spatial polygon object of convex hull}
#' @export
#'
#' @examples
#' \dontrun{
#' n    <- 3000
#' data <- data.frame(
#'   tt = sort(runif(min = 0,max = 2*pi,n = n))
#' )
#' data <- mutate(data,
#' xx = cos(tt) + rnorm(n = n,mean = 0,sd = .1) + 1*(tt > pi),
#' yy = sin(tt) + rnorm(n = n,mean = 0,sd = .1),
#' zz = cos(sqrt(tt)) + rnorm(n = n, mean = 0, sd = .1),
#' ww = yy * tt + rnorm(n = n, mean = 0, sd = .1)
#' )
#' 
#' data_points <- data %>% select(xx, yy)
#' # covex hull example
#' 
#' chull_out <- get_area_c(data_points)
#' c_points <- chull_out$poly
#' ggplot(data_points) + geom_point(aes(x = xx, y = yy)) +
#'   geom_path(data = c_points, 
#'             aes(x = xx, y = yy), color = "red")
#' }
get_area_c <- function(data, c_position = 1:2){
  data <- data[,c(c_position[2],c_position[1])]
  c_out <- grDevices::chull(data)
  c_points <- data[c_out,]
  
  poly <- rbind(c_points,c_points[1,])    # to ensure polygon is closed
  spPoly <- sp::SpatialPolygons(
                    list(sp::Polygons(list(sp::Polygon(poly)), ID = 1)))
  return(list(poly_df = poly, area = rgeos::gArea(spPoly), spPoly = spPoly))
}


#' Identification of points inside a spatial polygon
#' 
#' @description 
#' This function calculates whether a series of points are inside a given
#' polygon or not
#'
#' @param spPoly polygon object.
#' @param predict_mat matrix with the points to be determined in terms of 
#' position with respect to the contour.
#' @param long index of the column referring to "longitude". Default is 1.
#' @param lat index of the column referring to "latitude". Default is 2.
#
#' @return Vector of boolean values, if the point is inside the polygon, 
#' Dimensionality is the same as the number of rows in predict_mat
#' 
#' @details To account for computer storage of values generating rounding errors
#' this approach adds noise of \code{50 * .Machine$double.eps} in \emp{x} and 
#' \emp{y} directions and the point counts as interior if any of these jitters 
#' or the real point follow inside the polygon.
#' 
#' @references Inside this function we use \code{sp}'s \code{point.in.polygon}
#' with uses the C function InPoly(). InPoly is Copyright (c) 1998 by 
#' Joseph O'Rourke. It may be freely redistributed in its entirety provided 
#' that this copyright notice is not removed.
#' 
#' @examples
#' \dontrun{
#' set.seed(8192)
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' dfmat <- cbind(x1,y1)
#' kde_object <- ks::kde(dfmat)
#' cont <- with(kde_object, contourLines(x = eval.points[[1]],
#'                                       y = eval.points[[2]],
#'                                       z = estimate,levels = cont["5%"])[[1]])
#' 
#' poly <- with(cont, data.frame(x,y))
#' poly <- rbind(poly, poly[1, ])    # polygon needs to be closed
#' spPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(poly)),ID = 1)))
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' predict_mat <- cbind(x1,y1)
#' 
#' position_wrt_contour <- points_in_spatial_polygon(spPoly, predict_mat)
#'}
#' @export 
points_in_spatial_polygon <- function(spPoly, predict_mat, long = 1, lat = 2){

  eps2 <- 50 * .Machine$double.eps # to account for machine error 
  x_eps <- c(0, 0, 0, 1, -1) * eps2
  y_eps <- c(0, 1, -1, 0, 0) * eps2 
  points_in_poly <- sapply(1:4,
                           function(i) {sp::point.in.polygon(
                                          predict_mat[, lat] + x_eps[i], 
                                         predict_mat[, long] + y_eps[i],
                                spPoly@polygons[[1]]@Polygons[[1]]@coords[, 1],
                                spPoly@polygons[[1]]@Polygons[[1]]@coords[, 2])})
  
  points_in_poly <- apply(points_in_poly, MARGIN = 1, FUN = mean)
  
  return(as.numeric(points_in_poly > 0))
}



#' Convex Hull creation
#'
#' @param data_list list of hurricanes
#' @param alpha for credible band (related to depth)
#' @param dist_mat distance matrix (otherwise is calculated)
#' @param depth_vector Depth vector values
#' @param c_position Position of the longitude/latitude pair
#' @param data_deep_points data deep points from depth function 
#' (otherwise calculated)
#' @param verbose if the distance matrix is verbose
#' @param ... other parameters in distance calculation through 
#' \code{\link{distMatrixPath_innersq}}
#'
#' @return 
#' \item{polygon}{dataframe}
#' \item{size}{area of the convex hull}
#' \item{poly}{spatial polygon object of convex hull}
#' @export
convex_hull_structure <- function(data_list, alpha, dist_mat = NULL,
                                  data_deep_points = NULL, 
                                  depth_vector = NULL,
                                  c_position = 1:2,
                                  verbose = FALSE, ...){
  if (is.null(data_deep_points)) {
    # depth approach ---------------
    data_deep_points <- depth_curves_to_points(data_list, 
                                               alpha, 
                                               dist_mat = dist_mat,
                                               verbose = verbose,
                                               c_position = c_position,
                                               depth_vector = depth_vector,
                                               ...)
    
  }
  # getting convex hull structure and area -----------------
  
  chull_out <-  get_area_c(data_deep_points, c_position = 1:2)

  return(chull_out)
}
