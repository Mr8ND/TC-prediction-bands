#' Thirteen point compression
#'
#' @description Coverts list of point locations to 13 equally spaced points
#'
#' @param df2 2 column data frame with all points lat/lon 
#' @param lonlat boolean if the order of the columns is latlon 
#' (will be reverse them if FALSE)
#' @param output_length string of distance between points 
#' (set at "nautical mile" )
#'
#' @return new_13compression data.frame (13 x 2) of points along the path 
#' equally spaced
#' @export
#'
#' @examples
#' \dontrun{
#' df2 <-data.frame(matrix(
#'                  c(27.5, -87.5,
#'                  27.6, -87.9,
#'                  27.8, -88.2,
#'                  28.2, -88.4,
#'                  28.6, -88.6,
#'                  29.3, -89.0,
#'                  29.7, -89.4,
#'                  29.9, -89.5,
#'                  30.6, -90.4,
#'                  31.1, -91.6,
#'                  31.5, -92.8,
#'                  31.8, -93.8,
#'                  32.2, -94.7,
#'                  32.7, -95.0),ncol=2,byrow=T))
#' library(rworldmap)
#' newmap <- getMap(resolution = "low")
#' plot(newmap, ylim = c(10, 47), xlim = c(-90, -10), asp = 1)
#' 
#' lines(df2[,2], df2[,1], col = "red")
#' news_df2 <- thirteen_points(df2, lonlat = F)
#' points(news_df2[,1], news_df2[,2], col = "red")
#' }
thirteen_points <- function(df2, lonlat = TRUE,
                            output_length = "nautical mile"){
 
  if (lonlat == FALSE) { # reversing for geosphere functions
    df2 <- df2[,c(2,1)]
  }
  
  dist_and_bearing <- distAlongP(df2, longlat = TRUE)
  dist <- dist_and_bearing[[1]]
  bearing <- dist_and_bearing[[2]]
  
  total_dist <- sum(dist)
  step13 <- total_dist/12 # 12 equa-distance points along path
  cum_steps <- step13*(1:11)
  cum_dist <- cumsum(dist)[c(-length(dist))]
  
  new_13compression <- data.frame(matrix(0, nrow = 13, ncol = 2))
  index <- 2
  for (step in 1:length(cum_steps)) {
    step_full_dist <- cum_steps[step]
    start <- sum(cum_dist <= step_full_dist) + 1
    start_point <- df2[start,]
    start_bearing <- (bearing[start])
    if (start != 1) {
      step_dist <- step_full_dist - cum_dist[start - 1]
    }else{ # if no points other than the first is correct
      step_dist <- step_full_dist
    }    
    new_point <- geosphere::destPoint(start_point, start_bearing,
                           datamart::uconv(step_dist, output_length,
                                 "m", "Length"))
    new_13compression[index,] <- new_point
    index <- index + 1
  }
  new_13compression[1,] <- df2[1,]
  new_13compression[13,] <- df2[nrow(df2),]
  
  return(new_13compression)
}

#' List of 13 point compressions
#' 
#' @description Creates list of 13 point long/lat expression of each path
#'
#' @param list_df list of dfs, where the lonlat points are are in the 
#' c_position columns 
#' @param c_position the columns of the data frames that contain the desired 
#' lonlat coordinates 
#' @param lonlat boolean logical if columns are lonlat 
#' (false if they are latlon)
#' @param verbose boolean for having a progress bar
#'
#' @return List of 13 point compression data frame for each path
#' @export
thirteen_points_listable <- function(list_df, c_position = 5:6, lonlat = TRUE,
                                     verbose = TRUE){
  out_list <- list()
  n_tc = length(list_df)
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Compressing [:bar] :percent eta: :eta",
      total = n_tc, clear = FALSE, width = 38)
  }
  
  if (is.null(names(list_df))) {
    iterator_names <- 1:length(list_df)
  } else{
    iterator_names <- names(list_df)
  }

  for (path_name in iterator_names) {
    df_pulled_out <- list_df[[path_name]][,c_position]
    out_list[[path_name]] <- thirteen_points(df_pulled_out, lonlat)
    if (verbose) {
      pb$tick()
    }
  }
  
  return(out_list)
}