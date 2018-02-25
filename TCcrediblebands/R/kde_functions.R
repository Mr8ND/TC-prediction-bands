#' Flatten TC list to single dataframe
#' 
#' @description 
#' Converts list of dataframes to single dataframes row-wise
#'
#' @param dflist list of TC dataframes
#'
#' @return Resulting dataframe from a row-wise addition of all TC dataframes.
#' 
#' @examples
#' \dontrun{
#' set.seed(8192)
#' dflist <- list()
#' for (i in c(1:10)) {
#'  x <- 2^rnorm(100)
#'  y <- rnorm(100)
#'  dflist[[i]] <- cbind(x,y)
#' }
#' 
#' flatten_df <- flatten_tc_list(dflist)
#' dim(flatten_df)
#' }
flatten_tc_list = function(dflist) {

  dfmat <- dflist[[1]]
  for (i in c(2:length(dflist))) {
    dfmat <- rbind(dfmat, dflist[[i]])
  }

  return(dfmat)
}


#' Fit kernel density estimator object
#' 
#' @description 
#' This function fits a kernel density estimator to a TC dataframe, in which 
#' each point of the dataframe is considered to be a TC observation.
#'
#' @param dfmat TC dataframe
#' @param h_band optional argument for the bandwidth of the kde object. If NULL,
#' the optimal band would be selected through the \code{\link[ks]{kde}} 
#' function. Default is NULL.
#' @param long index of the column referring to "longitude". Default is 1.
#' @param lat index of the column referring to "latitude". Default is 2.
#' @param grid_size size of the grid which is going to be used for the 
#' evaluation of kde object. Can be reduced to speed-up computation.
#'
#' @return KDE object fitted to the dfmat dataframe.
#' 
#' @examples
#' \dontrun{
#' set.seed(8192)
#' x <- 2^rnorm(100)
#' y <- rnorm(100)
#' dfmat <- cbind(x,y)
#' 
#' kde_object <- fit_kde_object(dfmat)
#'}
fit_kde_object = function(dfmat, h_band = NULL, long = 1, lat = 2, 
                          grid_size = rep(1000,2)) {
  
  if (!is.null(h_band)) {
    h.mat <- diag(2)*h_band
    kde_obj <- ks::kde(dfmat[ ,c(long,lat)], gridsize = c(grid_size), H = h.mat)
  } else {
    kde_obj <- ks::kde(dfmat[ ,c(long,lat)], gridsize = c(grid_size))
  }
  return(kde_obj)
}


#' Evaluation of points with respect to KDE object
#' 
#' @description 
#' This function evaluates a matrix of geographical points with respect to a KDE
#' object.
#'
#' @param kde_obj kde object - in our case based on TC points 
#' (from \code{\link[ks]{kde}} )
#' @param predict_mat matrix with points to be predicted through KDE
#' @param alpha_level alpha level of the contour plot. Default is NULL. If not 
#' NULL, then an extra column will be added, in which 1 means that the value is 
#' above the alpha_level contour - i.e. within that probability contour - 
#' else 0 is returned.
#' @param long index of the column referring to "longitude". Default is 1.
#' @param lat index of the column referring to "latitude". Default is 2.
#
#' @return Matrix with prediction and, if alpha_level was not NULL, extra column
#' on whether the point is within a specific 1-alpha_level countour.
#'
#' @examples
#' \dontrun{
#' set.seed(8192)
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' dfmat <- cbind(x1,y1)
#' kde_object <- ks::kde(dfmat)
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' predict_mat <- cbind(x1,y1)
#' 
#' out_mat <- predict_kde_object(kde_object, predict_mat)
#' }
predict_kde_object = function(kde_obj, predict_mat, alpha_level = NULL, 
                              long = 1, lat = 2) {
  
  # The prediction mat is formatted and the prediction is performed
  predict_mat_kdefit <- predict_mat[ ,c(long,lat)]
  predict_vec <- stats::predict(kde_obj, x = predict_mat_kdefit, zero.flag = TRUE)
  out_mat <- cbind(predict_mat, predict_vec)
  
  # If the alpha level is selected, then the function will select the right 
  #level from the kde_obj$cont vector and store it for comparison.
  if (!is.null(alpha_level)) {
    contour_alpha_level <- as.numeric(kde_obj$cont[
                paste(as.character((1 - alpha_level) * 100), "%", sep = "")])
    in_alpha_vec <- as.numeric(predict_vec >= contour_alpha_level)
    out_mat <- cbind(out_mat, in_alpha_vec)
  }
  
  return(out_mat)
}


#' Selection of specific countour level from KDE object
#' 
#' @description 
#' This function extracts a specific level of contour from a kde object.
#' 
#' @details
#' Only works for some levels that the kde object calculated originally.
#'
#' @param kde_obj kde object
#' @param alpha_level contour level which needs to be extracted. Integer from 1 
#' to 99.
#
#' @return Countour at (100-level) for the kde object.
#' 
#' @examples
#' \dontrun{
#' set.seed(8192)
#' 
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' dfmat <- cbind(x1,y1)
#' kde_object <- ks::kde(dfmat)
#' 
#' cont <- extract_countour(kde_object, 5)
#' }
#' @export
extract_countour <- function(kde_obj, alpha_level) {
  alpha <- alpha_level*100
  cont_level <- paste0(as.character(alpha), "%")
  cont <- with(kde_obj, contourLines(x = eval.points[[1]],y = eval.points[[2]],
                                  z = estimate,levels = cont[cont_level])[[1]]) 
                                                        # ^needs to be 1-\alpha
  return(cont)
}


#' Area calculation of a kde contour object
#' 
#' @description 
#' This function calculates the area of a contour object by creating a closed
#' polygon and calculating the area of it.
#'
#' @param cont kde contour object.
#
#' @return Area of the kde contour object.
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
#'                                    y = eval.points[[2]],
#'                                    z = estimate,levels = cont["5%"])[[1]])
#' 
#' cont_area <- kde_contour_area(cont)
#'}
kde_contour_area <- function(cont) {
  poly <- with(cont, data.frame(x,y))
  poly <- rbind(poly, poly[1, ])    # polygon needs to be closed
  spPoly <- sp::SpatialPolygons(
                  list(sp::Polygons(list(sp::Polygon(poly)),ID = 1)))
  area <- rgeos::gArea(spPoly)
  
  return(area)
}


#' Identification of points inside a contour
#' 
#' @description 
#' This function calculates whether a series of points are inside a given
#' contour or not.
#'
#' @param cont kde contour object.
#' @param predict_mat matrix with the points to be determined in terms of
#' position with respect to the contour.
#' @param long index of the column referring to "longitude". Default is 1.
#' @param lat index of the column referring to "latitude". Default is 2.
#
#' @return Vector of binary values, 1 if the point is inside the contour, 
#' 0 is not. Dimensionality is the same as the number of rows in predict_mat
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
#'                                    y = eval.points[[2]],
#'                                    z = estimate,levels = cont["5%"])[[1]])
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' predict_mat <- cbind(x1,y1)
#' 
#' position_wrt_contour <- points_in_contour(cont, predict_mat)
#'}
points_in_contour <- function(cont, predict_mat, long = 1, lat = 2) {

  poly <- with(cont, data.frame(x,y))
  poly <- rbind(poly, poly[1, ])    # polygon needs to be closed
  spPoly <- sp::SpatialPolygons(
                list(sp::Polygons(list(sp::Polygon(poly)),ID = 1)))

  points_in_poly <- sp::point.in.polygon(predict_mat[, long], 
                                         predict_mat[, lat],
                              spPoly@polygons[[1]]@Polygons[[1]]@coords[, 1],
                              spPoly@polygons[[1]]@Polygons[[1]]@coords[, 2])

  return(points_in_poly)
}


#' Contour and area from TC list
#' 
#' @description
#' This function calculates contour points and area from list of generated TC.
#'
#' @param dflist list of TC dataframes
#' @param alpha_level contour level, an integer from 1 to 99. For example, a
#' level 5\% gives back 95\% contour
#' @param h_band optional argument for the bandwidth of the kde object. If NULL, 
#' the optimal band would be selected through the \code{\link[ks]{kde}} 
#' function. Default is NULL.
#' @param long index of the column referring to "longitude". Default is 1.
#' @param lat index of the column referring to "latitude". Default is 2.
#' @param grid_size size of the grid which is going to be used for the 
#' evaluation of kde object. Can be reduced to speed-up computation.
#
#' @return 
#' \item{contour}{Contour at the specified level}
#' \item{area}{Contour area}
#' \item{kde_object}{Full KDE Object (from \code{\link[ks]{kde}})}
#' 
kde_from_tclist <- function(dflist, alpha_level, h_band = NULL, 
                            long = 1, lat = 2,
                            grid_size = rep(1000,2)) {

  dfmat <- flatten_tc_list(dflist)
  kde_object <- fit_kde_object(dfmat, h_band = h_band, grid_size = grid_size, 
                                long = long, lat = lat)
  cont <- extract_countour(kde_object, alpha_level = alpha_level)
  area_cont <- kde_contour_area(cont)

  return(list('contour' = cont, 'area' = area_cont, 'kde_object' = kde_object))
}