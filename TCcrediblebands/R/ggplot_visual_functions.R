# Basic visualization functions -----------------------


#' Format curves from a list to a single data.frame
#'
#' @param test_list list of curves
#' @param c_position position of lat and lon columns in the test_list dataframes
#'
#' @return single data frame with all curves (curve column with idx of curve)
#' @export
#'
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
#' @return ggplot visualisation of the curve
#' @export
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
                                  ggplot2::aes_string(x = 'long', y = 'lat', group = 'curve'),
                                  alpha = alpha) 
  
  return(ggout)
}


# KDE contour visualization functions -----------------------

#' Create level set for kde
#' 
#' @description
#' Returns the level set for a given kde
#'
#' @param kde.obj kde object 
#' @param level string of percentage level outside the contour
#'
#' @return list of data frame of countour (detail determined by 
#' detail level in kde.obj) - there is a contour for each 
#' disconnected contour that forms the (100-level) contour
#' level.
#' @export
get_kde_contour_path <- function(kde.obj, level = "5%"){
  level_contour <- with(kde.obj, contourLines(x = eval.points[[1]], 
                                              y = eval.points[[2]],
                                              z = estimate, 
                                              levels = cont[level]))
  level_contour <- lapply(level_contour, data.frame)
  
  return(level_contour)
}
  

#' Create ggplot of contour
#' 
#' @param level_contour_df data frame of level contour
#' @param data_plot data frame of points
#' @param base_graph ggplot object for base graph 
#'       (created from data_out otherwise)
#' @param zoom map zoom for ggmap
#'
#' @return ggplot object of contour and data points.
#' @export
ggvis_kde_contour <- function(level_contour_df, data_plot, base_graph = NULL,
                              zoom = 4){
  
  if (is.null(base_graph)) {
    latrange <- range(data_plot$lat)
    lonrange <- range(data_plot$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
  } 
  
  ggout <- base_graph  +
    ggplot2::geom_point(data_plot, ggplot2::aes_string(x = 'long', y = 'lat')) +
    ggplot2::geom_path(ggplot2::aes_string(x = 'x', y = 'y'), data = level_contour_df)
  
  return(ggout)
}


# Delta Ball Visualization function ------------------------

#' Visualize delta ball exterior centers (with ggplot)
#'
#' @param output_lines data frame of exterior lines (not ordered)
#' @param base_graph ggplot object for base graph 
#'       (created from data_out otherwise)
#' @param zoom map zoom for ggmap
#'
#' @return ggplot object of contour and data points.
#' @export
#'
gg_vis_delta_ball_contour <- function(output_lines, base_graph = NULL, zoom = 4){
  
  if (is.null(base_graph)) {
    latrange <- range(output_lines$lat)
    lonrange <- range(output_lines$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
  } 
  
  ggout <- base_graph + 
    ggplot2::geom_line(data = output_lines, ggplot2::aes_string(x = 'lat', y = 'lon', group = 'idx'))
  
  return(ggout)
}

# Bubble visualization functions -------------------------


#' Plot lines for bubble data 
#'
#' @param bubble_plot_data list of center points, lower and upper tangential 
#' points 
#' (each a n x 2 data frame)
#' @param base_graph plot to add band to
#' @param color color of band
#' @param linewidth width of line
#' @param zoom map zoom for ggmap
#' @param ... Interior ggpath parameters
#'
#' @return ggplot object with curves on it
#' @export
#'
bubble_data_plot_lines <- function(bubble_plot_data, base_graph = NULL,
                                   color = "pink", linewidth = 1, zoom = 4,
                                   ...){
  data_plot_lower <- bubble_plot_data$lower
  names(data_plot_lower)[1:2] <- c("lat", "lon")
  data_plot_upper <- bubble_plot_data$upper
  names(data_plot_upper)[1:2] <- c("lat", "lon")
  
  if (is.null(base_graph)) {
    
    data_all <- rbind(data_plot_lower, data_plot_upper)
    latrange <- range(data_all$lat)
    lonrange <- range(data_all$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
    
  } 
  
  ggout <- base_graph + 
    ggplot2::geom_path(data = data_plot_lower, ggplot2::aes_string(x = 'lat', y = 'lon'), 
              color = color, linewidth = linewidth, ...) +
    ggplot2::geom_path(data = data_plot_upper, ggplot2::aes_string(x = 'lat', y = 'lon'), 
              color = color, linewidth = linewidth, ...) 
  
  return(ggout)
}


#' Plot centers for bubble data
#'
#' @param bubble_plot_data list of center points, lower and upper tangential 
#' points (each a n x 2 data frame)
#' @param base_graph plot to add points to
#' @param color color of points
#' @param zoom map zoom for ggmap
#' @param ... Interior ggpath parameters
#'
#' @return ggplot object with points on it
#' @export
#'
bubble_data_plot_prep_center <- function(bubble_plot_data, base_graph = NULL, 
                                         color = "pink", zoom = 4, ...){
  
  center <- data.frame(bubble_plot_data$center)
  names(center)[1:2] <- c("lat", "lon")
  
  if (is.null(base_graph)) {
    latrange <- range(center$lat)
    lonrange <- range(center$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
  } 
  
  ggout <- base_graph + 
    ggplot2::geom_point(data = center, ggplot2::aes_string(x = 'lat', y = 'lon'),
               color = color, ...)
  
  return(ggout)
}
