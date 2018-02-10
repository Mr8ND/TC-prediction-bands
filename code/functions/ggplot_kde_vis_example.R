library(rgeos)
library(ggmap)
library(tidyverse)
library(ks)

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

#' Create level set fro kde
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
#' TODO: decide on desired input - probably a base plot
#'
#' @param level_contour_df data frame of level contour
#' @param data_plot data frame of points
#'
#' @return ggplot object of contour and data points.
#' @export
#'
#' @examples
ggvis_kde_contour <- function(level_contour_df, data_plot){
  ggout <- ggplot(data_plot, aes(x = long, y = lat)) +
    geom_point() +
    geom_path(aes(x, y), data = level_contour_df)
  
  return(ggout)
}


###################
# Current example #
###################

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


