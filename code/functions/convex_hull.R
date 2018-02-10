# TODO: figure out the commented out code - is it an example?
# has an example of really cool progress bar --> we should use ;P
library(tidyverse)
library(ks)
library(sp)
library(rgeos)
library(geosphere)
library(progress) 

#' Get area of convex hull
#'
#' @param data points to find a convex hull over (assumed n x 2)
#'
#' @return polygon dataframe
#' @return size area of the convex hull
#' @return poly spatial polygon object of convex hull
#' @export
#'
#' @examples 
#' library(tidyverse)
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
#' 
get_area_c <- function(data){
  c_out <- chull(data_points)
  c_points <- data_points[c_out,]
  
  poly <- rbind(c_points,c_points[1,])    # polygon needs to be closed...
  spPoly <- SpatialPolygons(list(Polygons(list(Polygon(poly)),ID = 1)))
  return(list(poly_df = poly, size = gArea(spPoly), spPoly = spPoly))
}


#' Identification of points inside a spatial polygon
#' 
#' @description 
#' This function calculates whether a series of points are inside a given
#' polygon or not
#'
#' @param spPoly polygon object.
#' @param predict_mat matrix with the points to be determined in terms of position
#' with respect to the contour.
#' @param long index of the column referring to "longitude". Default is 1.
#' @param lat index of the column referring to "latitude". Default is 2.
#
#' @return Vector of binary values, 1 if the point is inside the polygon, 0 is not.
#' Dimensionality is the same as the number of rows in predict_mat
#' 
#' @examples
#' set.seed(8192)
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' dfmat <- cbind(x1,y1)
#' kde_object <- kde(dfmat)
#' cont <- with(kde_object, contourLines(x = eval.points[[1]],y = eval.points[[2]],
#'                                    z = estimate,levels = cont["5%"])[[1]])
#' 
#' poly <- with(cont, data.frame(x,y))
#' poly <- rbind(poly, poly[1, ])    # polygon needs to be closed
#' spPoly <- SpatialPolygons(list(Polygons(list(Polygon(poly)),ID = 1)))
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' predict_mat <- cbind(x1,y1)
#' 
#' position_wrt_contour <- points_in_spatial_polygon(spPoly, predict_mat)
#'
points_in_spatial_polygon <- function(spPoly, predict_mat, long = 1, lat = 2) {

  points_in_poly <- point.in.polygon(predict_mat[, long], predict_mat[, lat],
                                    spPoly@polygons[[1]]@Polygons[[1]]@coords[, 1],
                                    spPoly@polygons[[1]]@Polygons[[1]]@coords[, 2])

  return(points_in_poly)
}


# 
# n    <- 3000
# data <- data.frame(
#    tt = sort(runif(min = 0,max = 2*pi,n = n))
# )
#    
# data <- mutate(data,
#  xx = cos(tt) + rnorm(n = n,mean = 0,sd = .1) + 1*(tt > pi),
#  yy = sin(tt) + rnorm(n = n,mean = 0,sd = .1),
#  zz = cos(sqrt(tt)) + rnorm(n = n, mean = 0, sd = .1),
#  ww = yy * tt + rnorm(n = n, mean = 0, sd = .1)
#  )
#  
# data_points <- data %>% select(xx, yy)
#  # covex hull example
#  
#  chull_out <- get_area_c(data_points)
#  c_points <- chull_out$poly
#  ggplot(data_points) + geom_point(aes(x = xx, y = yy)) +
#    geom_path(data = c_points, 
#              aes(x = xx, y = yy), color = "red")
# 
# D_mat <- as.matrix(dist(data_points))
# diag(D_mat) <- max(D_mat)
# delta <- max(apply(D_mat,1, min))
# 
# ##kernel2dsmooth requires the full space in a matrix
# 
# one_vs_more2d <- function(x, xx){
#   apply(xx, 1, function(y) sqrt(sum((y - x)^2)))
# }
# 
# map2d_disk <- function(data, radius = 1, nx = 100, ny = 100,
#                        verbose = T){
#   if (verbose) {
#     pb <- progress_bar$new(
#       format = "  Processing [:bar] :percent eta: :eta",
#       total = 1000, clear = FALSE, width = 40)
#   }
#   
#   grid <- matrix(nrow = ny, ncol = nx)
# 
#   x_range <- range(data[,1])
#   xx      <- seq(x_range[1], x_range[2], length.out = nx)
#   y_range <- range(data[,2])
#   yy      <- seq(y_range[1], y_range[2], length.out = ny)
# 
#   for (x_index in 1:nx) {
#     for (y_index in 1:ny) {
#       grid[x_index, y_index] <- 1*any(
#         one_vs_more2d(c(xx[x_index],yy[y_index]), data) < radius
#         )
#     }
#     
#     if (verbose) {
#       pb$tick()
#     }
#   }
# 
#   return(list(grid = grid, x = xx, y = yy))
# }
# 
# grid <- map2d_disk(data_points, radius = delta/2)
