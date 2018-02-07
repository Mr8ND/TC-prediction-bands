## this function file should be removed/ not used if possible 
# (currently needed for loocv)

##### Libraries
library(geosphere)
library(plyr)


#### Loading inner functions
functions_loc <- "code/functions/"
source(file = paste0(functions_loc,"Path_functions.R"))
source(file = paste0(functions_loc,"Regression_functions.R"))

############
# Function #
############

#' Coverts list of point locations to 13 points equally spaced apart
#'
#' @param df2 2 column data frame with all points lat/lon 
#' @param lonlat if the order of the columns is latlon 
#' (will be reverse if FALSE)
#' @param output_length string of distance between points 
#' (set at "nautical mile" )
#'
#' @return new_13compression data.frame (13 x 2) of points along the path 
#' equally spaced
#' @return new_12speed data.frame (12 x 1) of speeds along above path
#' (not actually sure the is correct - but we ended up not using it)
#' @export
#'
#' @examples
#'  # visual testing of function
#'  # should be able to make df2 any lat long matrix from the training data:
#'  df2 <- data.frame(matrix(
#'                   c(27.5, -87.5,
#'                   27.6, -87.9,
#'                   27.8, -88.2,
#'                   28.2, -88.4,
#'                   28.6, -88.6,
#'                   29.3, -89.0,
#'                   29.7, -89.4,
#'                   29.9, -89.5,
#'                   30.6, -90.4,
#'                   31.1, -91.6,
#'                   31.5, -92.8,
#'                   31.8, -93.8,
#'                   32.2, -94.7,
#'                   32.7, -95.0),ncol=2,byrow=T))
#'  
#'  library(rworldmap) 
#'  newmap <- getMap(resolution = "low")
#'  plot(newmap, ylim = c(10, 47), xlim = c(-90, -10), asp = 1)
#'  
#'  lines(df2[,2], df2[,1], col = "red")
#'  news_df2 <- compression_points(df2, lonlat = F)
#'  points(news_df2[,1], news_df2[,2], col = "red")
compression_points <- function(df2, lonlat = TRUE, 
                               output_length = "nautical mile"){
  ###########
  # lat lon #
  ###########

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
  
  #########
  # speed #
  #########

  speed_df <- df2
  names(speed_df) <- c("long","lat")
  speed_df <- append.speed(speed_df)
  speed    <- speed_df$speed
  speed[length(speed)] <- speed[length(speed) - 1] 
  # ^ correct for NA in the last position

  new_12speed  <- data.frame(matrix(0,nrow = 12, ncol = 1))    
  


  index = 2
  for (step in 1:length(cum_steps)) {
    ###########
    # lat lon #
    ###########
    step_full_dist <- cum_steps[step]
    start <- sum(cum_dist <= step_full_dist) + 1
    start_point <- df2[start,]
    start_bearing <- (bearing[start])
    if (start != 1) {
      step_dist <- step_full_dist - cum_dist[start - 1]
    }else{# if no points other than the first is correct
      step_dist <- step_full_dist
    }    
    new_point <- destPoint(start_point, start_bearing,
                           uconv(step_dist, output_length,
                                 "m", "Length"))
    new_13compression[index,] <- new_point
    

    #########
    # speed # 
    #########
    frac = step_dist/dist[start]
     if (frac > 1) {
      print("you coded it wrong")
    }
    new_12speed[index,1] <- frac * speed[start] + (1 - frac) * speed[start + 1]


    ################
    # index update #
    ################
    index <- index + 1
  }
  new_13compression[1,] <- df2[1,]
  new_13compression[13,] <- df2[nrow(df2),]

  #########
  # speed # 
  #########

  new_12speed[1,1] <- speed[1]

  return(list(new_13compression = new_13compression,
              new_12speed = new_12speed))
}

#' Creates list of 13 point lonlat expression of each path
#'
#' @param list_df list of dfs, where the lonlat points are are in the
#' c_position columns 
#' @param c_position the columns of the data frames that contain the 
#' desired lonlat coordinates
#' @param lonlat logical if columns are lonlat (false if they are latlon)
#'
#' @return
#' @export
#'
#' @examples
compression_points_listable <- function(list_df, c_position = 5:6,
                                        lonlat =TRUE){
  out_list <- list()
  for (i in 1:length(list_df)) {
    df_pulled_out <- list_df[[i]][,c_position]
    out_list[[i]] <- compression_points(df_pulled_out, lonlat)
  }
  
  return(out_list)
  
}

