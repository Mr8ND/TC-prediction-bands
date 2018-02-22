library(rgeos)
library(geosphere) 
library(ggmap)
library(tidyverse)
library(ks)


# Locations ------------------------

project_location      <- ""
functions_loc         <- "code/functions/"

desired_functions <- c("path_functions.R",
                       "thirteen_point_compression.R",
                       "projection_map.R",
                       "depth_function.R")

# Loading functions ------------------------

for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name)) #needs at least calculateErrorBandsBubble
}


# Basic visualization functions -----------------------


#' Format curves from a list to a single data.frame
#'
#' @param test_list list of curves
#' @param c_position position of lat and lon columns in the test_list dataframes
#'
#' @return single data frame with all curves (curve column with idx of curve)
#' @export
#'
#' @examples
data_plot_paths_basic <- function(test_list, c_position = 1:2) {
  
  data_out <- data.frame(lat = -360,
                         long = -360,
                         curve = 0)
  
  for (i in 1:length(test_list)) {
    data_out <- rbind(data_out,
                      data.frame(
                        lat = test_list[[i]][, c_position[2]],
                        long = test_list[[i]][, c_position[1]],
                        curve = i
                      ))
  }
  data_out <- data_out[-1, ]
  
  return(data_out)
  
}

#' Visualize TC Paths (with ggplot)
#'
#' Note / TODO: 
#' Function currently uses geom_path - assumed euclidean space for map :/
#'
#' @param data_out data frame with correct path information same as outputed 
#'       from data_plot_sc_paths functions
#' @param zoom map zoom for ggmap
#' @param base_graph ggplot object for base graph 
#'       (created from data_out otherwise)
#' @param alpha opacity of curves 
#'
#' @return
#' @export
#'
#' @examples
ggvis_paths <- function(data_out, zoom = 4,
                        base_graph = NULL, alpha = .01){
  if (is.null(base_graph)) {
    latrange <- range(data_out$lat)
    lonrange <- range(data_out$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
  } 
  
  # final map ---------------
  ggout <- base_graph + ggplot2::geom_path(data = data_out, 
                                  aes(x = long, y = lat, group = curve),
                                  alpha = alpha) 
  
  return(ggout)
}


# KDE contour visualization functions -----------------------

#' Create level set for kde
#'
#' @param kde.obj kde object 
#' @param level string of percentage level outside the contour, ex "5%"
#'
#' @return data frame of countour (detail determined by detail level in kde.obj)
#' @export
#'
#' @examples
get_kde_contour_path <- function(kde.obj, level = "5%"){
  level_contour <- with(kde.obj, contourLines(x = eval.points[[1]], 
                                              y = eval.points[[2]],
                                              z = estimate, 
                                              levels = cont[level])[[1]])
  level_contour <- data.frame(level_contour)
  
  return(level_contour)
}
  

#' Create ggplot of contour
#' 
#' @param level_contour_df data frame of level contour
#' @param data_plot data frame of points
#' @param base_graph ggplot object for base graph 
#'       (created from data_out otherwise)
#'
#' @return ggplot object of contour and data points.
#' @export
#'
#' @examples
ggvis_kde_contour <- function(level_contour_df, data_plot, base_graph = NULL){
  
  if (is.null(base_graph)) {
    latrange <- range(data_out$lat)
    lonrange <- range(data_out$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
  } 
  
  ggout <- base_graph  +
    ggplot2::geom_point(data_plot, aes(x = long, y = lat)) +
    ggplot2::geom_path(aes(x, y), data = level_contour_df)
  
  return(ggout)
}


# Delta Ball Visualization function ------------------------

#' Visualize delta ball exterior centers (with ggplot)
#'
#' @param output_lines data frame of exterior lines (not ordered)
#' @param base_graph ggplot object for base graph 
#'       (created from data_out otherwise)
#'
#' @return ggplot object of contour and data points.
#' @export
#'
#' @examples
gg_vis_delta_ball_contour <- function(output_lines, base_graph = NULL){
  
  if (is.null(base_graph)) {
    latrange <- range(output_lines$lat)
    lonrange <- range(output_lines$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
  } 
  
  ggout <- base_graph + 
    ggplot2::geom_line(data = output_lines, aes(x = lat, y = lon, group = idx))
  
  return(ggout)
}

# Bubble visualization functions -------------------------


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
  names(data_plot_lower)[1:2] <- c("lat", "lon")
  data_plot_upper <- bubble_plot_data$upper
  names(data_plot_upper)[1:2] <- c("lat", "lon")
  
  if (is.null(gg_add)) {
    
    data_all <- rbind(data_plot_lower, data_plot_upper)
    latrange <- range(data_all$lat)
    lonrange <- range(data_all$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    gg_add <- ggmap::ggmap(map)
    
  } 
  
  out <- gg_add + 
    ggplot2::geom_path(data = data_plot_lower, aes(x = lat, y = lon), 
              color = color, linewidth = linewidth, ...) +
    ggplot2::geom_path(data = data_plot_upper, aes(x = lat, y = lon), 
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
  names(center)[1:2] <- c("lat", "lon")
  
  if (is.null(gg_add)) {
    latrange <- range(center$lat)
    lonrange <- range(center$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    gg_add <- ggmap::ggmap(map)
  } 
  
  out <- gg_add + 
    ggplot2::geom_point(data = center, aes(x = lat, y = lon),
               color = color, ...)
}


# KDE example -------------------------

# dat_names <- load("~/Desktop/Val_Sims_2500.Rdata")

# test <- val_env$AL212003$Auto_DeathRegs

# test2 <- list()
# for (i in 1:100) {
#   test2[[i]] <- test[[i]]
# }

# test <- test2

# data_plot <- test %>% data_plot_paths_basic()

# head(data_plot)

# kde.obj <- kde(data_plot[, 2:1], gridsize = 1000)



# Delta Ball Example -----------------------

# dat_names <- load("~/Desktop/Val_Sims_2500.Rdata")

# test <- val_env$AL212003$Auto_DeathRegs

# test2 <- list()
# for (i in 1:100) {
#   test2[[i]] <- test[[i]]
# }

# test <- test2

# data_plot <- test %>% data_plot_paths_basic()


# data_plot <- test %>% data_plot_paths_basic() 


# gout <- data_plot %>% ggvis_paths_basic(alpha = 1)

# gout
# # depth function should select curves to keep, then pipe data_plot into 
# # delta_ball_wrapper

# outline <- data_plot %>% delta_ball_wrapper(n_steps = 300, 
#                                             remove_duplicates = T)


# gout + geom_path(data = outline, aes(x = y, y = x, group = idx),
#                  color = "red")

# quartz()
# ggplot() + geom_path(data = outline, aes(x = y, y = x, group = idx),
#                      color = "red")
