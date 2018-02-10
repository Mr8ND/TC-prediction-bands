library(geosphere) 
library(tidyverse)
library(ggmap)
library(GGally)
library(gridExtra)

#############
# Locations #
#############

project_location      <- ""
functions_loc         <- "code/functions/"

desired_functions <- c("Path_functions.R","13pointreduction.R",
                      "projection_map.R","visualizing_color_gradient.R",
                      "depth_function.R","bubble_points_functions2.R")

#####################
# Loading functions #
#####################

for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name)) #needs at least calculateErrorBandsBubble
}


#' Title
#'
#' @param bubble_step same input as cacluateErrorBandsBubble needs (TODO)
#'
#' @return center data n x 2 points for center path of bubble
#' @return lower data n x 2 points for "lower" tangental point a center path
#' @return upper data n x 2 points for "upper" tangental point a center path
#' 
#' @export
#'
#' @examples
bubble_data_plot_prep <- function(bubble_step){

  NSWE_lists <- calculateErrorBandsBubble(bubble_step, conversion = TRUE)
  error_NS      <- NSWE_lists[[1]]
  error_EW      <- NSWE_lists[[2]]
  center_radius <- NSWE_lists[[3]]
  
  lower_bound_errors <- c()
  upper_bound_errors <- c()
  for (i in c(1:length(error.EW))) {
    lower_bound_errors <- c(lower_bound_errors,error_EW[[i]][c(1,2)])
    upper_bound_errors <- c(upper_bound_errors,error_EW[[i]][c(3,4)])
  }
  lower_bound_errors_df <- data.frame(t(matrix(lower_bound_errors, nrow = 2)))
  upper_bound_errors_df <- data.frame(t(matrix(upper_bound_errors, nrow = 2)))
  
  return(list(center = center_radius,
              lower = lower_bound_error_df,
              upper = upper_bound_errors_df))
}

#' Plot lines for bubble data 
#'
#' @param bubble_plot_data list of center points, lower and upper tangential 
#' points from `bubble_data_plot_prep`` function (each a n x 2 data frame)
#' @param gg_add plot to add band to
#' @param color color of band
#' @param linewidth width of line
#'
#' @return ggplot object with curves on it
#' @export
#'
#' @examples
bubble_data_plot_lines <- function(bubble_plot_data,gg_add = NULL,
                                     color = "pink", linewidth = 1, ...){
  data_plot_lower <- bubble_plot_data$lower
  names(data_plot_lower)[1:2] <- c("lat", "long")
  data_plot_upper <- bubble_plot_data$upper
  names(data_plot_upper)[1:2] <- c("lat", "long")

  if (is.null(gg_add)) {
    
    data_all <- rbind(data_plot_lower, data_plot_upper)
    latrange <- range(data_all$lat)
    lonrange <- range(data_all$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    gg_add <- ggmap(map)
    
  } 

  out <- gg_add + 
            geom_path(data = data_plot_lower, aes(x = lat, y = long), 
                      color = color, linewidth = linewidth, ...) +
            geom_path(data = data_plot_upper, aes(x = lat, y = long), 
                      color = color, linewidth = linewidth, ...) 

  return(out)
}


#' Plot centers for bubble data
#'
#' @param bubble_plot_data list of center points, lower and upper tangential 
#' points from `bubble_data_plot_prep`` function (each a n x 2 data frame)
#' @param gg_add plot to add points to
#' @param color color of points
#'
#' @return ggplot object with points on it
#' @export
#'
#' @examples
bubble_data_plot_prep_center <- function(bubble_plot_data, gg_add = NULL, 
                                         color = "pink", ...){

  center <- data.frame(bubble_plot_data$center)
  names(center)[1:2] <- c("lat", "long")

  
  if (is.null(gg_add)) {
    latrange <- range(center$lat)
    lonrange <- range(center$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    gg_add <- ggmap(map)
    
  } 
  
   out <- gg_add + 
            geom_point(data = center, aes(x = lat, y = long),
                       color = color, ...)
}



