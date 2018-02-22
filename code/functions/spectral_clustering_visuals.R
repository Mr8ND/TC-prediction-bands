library(tidyverse)
library(ggmap)
library(GGally)
library(gridExtra)
# functions to take paths and visual them related to observed paths with 
# spectral clustering

## need to call in distMatrixPath_t2t_path from Path_functions.R
source("code/functions/Path_functions.R")
source("code/functions/13pointreduction.R")
source("code/functions/point_reduction_with_speed.R")


# takes list of compressed points (via compression_points_listable)
# actually should be a list of just the new_13compression

### from second_pipeline.R

#' Spectral Clustering Process
#' 
#' produces weights and projection for paths (from projection)
#'
#' @param test_list list of paths to analysis (each a df)
#' @param train_df 13 point compression of training curves
#' @param D_train distance matrix from training curves
#' @param K nearest neighbors for spectral clustering inside smoothing
#' @param t power to raise spectral cluster transition matrix
#' @param dim dimension of lower dimensional projection from spectral c.
#' @param kdensity nearest neighbors for density estimate in lower dim space
#' @param c_position nearest neighbors for density estimate in lower dim space
#'
#' @return test_p_estimate probability estimates for test data
#' @return test_weight scaled probabilities estimates (by max of probs) for test
#' @return test_13point 13 point compression for test data
#' @return test_projected test data in projection space
#' @return train_projected training data in projection space
#' 
#' @export
#'
#' @examples
spectral_cluster_process <- function(test_list, train_df, D_train,
							 K = 4, t = 1, dim = 5, kdensity = 10,
							 c_position = 1:2){

	compression_both <- compression_points_listable(list_df = test_list,
	                            c_position = c_position,lonlat = TRUE)

	compression_pts <- lapply(compression_both, 
								function(x){x$new_13compression})

	D_test <- distMatrixPath_t2t_path(
					path_mat_list_test = compression_pts,
					path_mat_list_train = train_df) 

	#^- should create matrix comparing training to ordered test matrix 
	# (13 point compression)
	# second_pipeline.R 121 stores a list for some reason

	training_structure <- training_structure_estimating_p(D_train,
														  K = 4,
														  t = 1,
														  dim = 5,
														  plot_n = 0)
	
	estimate_p <- estimate_p_wrapper(
					training_structure_estimating_p = training_structure,
	                D_test = D_test,
	                kdensity = kdensity)

	output <- list()
	output[["test_p_estimate"]] <- estimate_p$p_estimate_test$p
	output[["test_weight"]] <- estimate_p$p_estimate_test$p /
	                              max(estimate_p$p_estimate_test$p)
	output[["test_13point"]] <- compression_pts
	output[["test_projected"]] <- estimate_p$test_projected
	output[["train_projected"]] <- training_structure$train_projected

	return(output)
}


#' Compresses data from list (to be plotted in ggplot)
#' 
#' compresses data into tidyverse focused dataframe (for ggplot)
#'
#' @param test_list list of paths to analysis (each a df)
#' @param scp_output spectral_cluster_process function output
#' @param c_position positions of lat and lon columns in test_list data frames
#'
#' @return data_out data frame that can be used to visualize curves
#' @export
#'
#' @examples
data_plot_sc_paths <- function(test_list, scp_output, c_position = 1:2){

	data_out <- data.frame(lat = -360, long = -360, prob = 0, 
						   weight = 0, curve = 0)

	for (i in 1:length(test_list)) {
		data_out <- rbind(data_out,
						  data.frame(lat = test_list[[i]][c_position[2]],
						  			 long = test_list[[i]][c_position[1]],
						  			 prob = scp_output$test_p_estimate[i],
						  			 weight = scp_output$test_weight[i],
						  			 curve = i))
	}
	data_out <- data_out[-1,]

	return(data_out)
}


#' Visualize the TC paths with weights colored
#'
#'	Note / TODO: 
#'	1. Function currently uses geom_path - assumed euclidean space for map :/
#'
#' @param data_out data frame with correct path information same as outputed 
#' from data_plot_sc_paths functions
#' @param zoom map zoom for ggmap
#' @param test_color_power power to raise the probability weights 
#' (then use linear scaling for colors)
#' @param test_color_low lower color value for color range, 
#' @param test_color_high higher color value for color range,
#' @param base_graph ggplot object for base graph 
#' (created from data_out otherwise)
#'
#' @return ggmap based map object
#' @export
#'
#' @examples
ggvis_paths_sca_weight <- function(data_out, zoom = 4,
						test_color_power = 1/3, 
						test_color_low = "white",
						test_color_high = "red",
						base_graph = NULL){

	if (is.null(base_graph)) {
		latrange <- range(data_out$lat)
		lonrange <- range(data_out$long)

		ocean <- c(left = lonrange[1], bottom = latrange[1],
					right = lonrange[2], top = latrange[2])
		map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
		
		base_graph <- ggmap::ggmap(map)

	} 

	## coloring
	color_out <- color_function(data_out$weight,
							test_color_power = test_color_power,
							test_color_low = test_color_low,
							test_color_high = test_color_high)

	data_out <- data_out %>% mutate(weight_discrete = color_out$breaks)
	colors_rw <- color_out$colors_rw


	# final map:

	ggout <- base_graph + ggplot2::geom_path(data = data_out, 
	                                         aes(x = long, y = lat, 
	                                             color = weight_discrete, 
	                                             group = curve)) +
				ggplot2::scale_color_manual(values = colors_rw) +
				ggplot2::labs(color = paste0("weights^(",round(test_color_power,2),")"))

	return(ggout)
}

#' Get points for objects in projection space
#'
#' @param scp_output pectral_cluster_process function output
#'
#' @return test_df data frame of training points and probability weights
#' @return train_df data frame of test points
#' @export
#'
#' @examples
data_projection <- function(scp_output){
	n <- dim(scp_output$test_projected)[1]
	test_df <- scp_output$test_projected %>% data.frame %>% 
				dpylr::mutate(prob = scp_output$test_p_estimate,
					   weight = scp_output$test_weight,
					   curve = 1:n)
	train_df <- scp_output$train_projected %>% data.frame

	return(list(test_df = test_df, train_df = train_df))
}


#' Inner color function setup
#'
#' @param weights_in probability weights for 
#' @param test_color_power power to raise the probability weights 
#' (then use linear scaling for colors)
#' @param test_color_low lower color value for color range, 
#' @param test_color_high higher color value for color range,
#'
#' @return breaks vector of which of the 10 breaks each weight is in 
#' (relative to power transformation)
#' @return colors_rw color palette ramp vector
#'
#' @examples
color_function <- function(weights_in,
							test_color_power = 1/3,
							test_color_low = "white",
							test_color_high = "red"){

	w_func <- function(x){return(x^test_color_power)}

	breaks <- cut(w_func(weights_in), breaks = c(0, 1:10/10))
	colors_rw <- grDevices::colorRampPalette(c(test_color_low, 
	                                           test_color_high))(10)

	return(list(breaks = breaks, colors_rw = colors_rw))
}

#' Creates ggplot of projections
#'
#' Note:
#' 	ggplot experts are encouraged to use the output of data_projection 
#' 	function applied to scp_output instead of this wrapper
#'
#' @param scp_output spectral_cluster_process function output
#' @param train_alpha opacity level for black training points
#' @param test_color_power power transformation (x^ test_color_power) of 
#' probability values for test points color
#' @param test_color_low lower color for range of colors on test points prob
#' @param test_color_high high color for range of colors on test points prob
#'
#' @return ggplot scatter plot of training and colored test points
#' @export
#'
#' @examples
ggvis_projection <- function(scp_output, train_alpha = .3, 
							 test_color_power = 1/3, 
							 test_color_low = "white",
							 test_color_high = "red"){
	# data
	data_p <- data_projection(scp_output)
	test_df <- data_p$test_df
	train_df <- data_p$train_df

	# color setup

	color_out <- color_function(test_df$weight,
							test_color_power = test_color_power,
							test_color_low = test_color_low,
							test_color_high = test_color_high)

	test_df <- test_df %>% dplyr::mutate(weight_discrete = color_out$breaks)
	colors_rw <- color_out$colors_rw


	ggout <- ggplot2::ggplot() +
				ggplot2::geom_point(data = train_df, aes(x = X1, y = X2),
						   alpha = train_alpha, color = 'black') +
	      ggplot2::geom_point(data = test_df, 
						   aes(x = X1, y = X2, fill = weight_discrete),
						   shape = 21, color = "black") +
	      ggplot2::scale_fill_manual(values = colors_rw) + 
	      ggplot2::labs(fill = paste0("weights^(",round(test_color_power,2),")"))


    return(ggout)
}




#' 	Creates clean visual of weighted curves using Spectral Clustering Analysis
#' 	Specifically, the projected points in 2d and the paths colored
#'
#' Note:
#' ggplot experts are encouraged to use the output of this function to  
#' instead of just running with the created plot
#'
#' @param scp_output spectral_cluster_process function output
#' @param test_list list of paths to analysis (each a df)
#' @param c_position positions of lat and lon columns in test_list data frames
#' @param zoom map zoom for ggmap (plot 2: map)
#' @param train_alpha opacity level for black training points (plot 1: scatter)
#' @param test_color_power power transformation (x^ test_color_power) of 
#' probability values for test points color (plot 1: scatter)
#' @param test_color_low lower color for range of colors on test points prob
#' (plot 1: scatter)
#' @param test_color_high high color for range of colors on test points prob
#' (plot 1: scatter)
#'
#' @return gg_path ggmap based map object of colored test curves
#' @return gg_proj ggplot scatter plot of training and colored test points
#' 
#' also visualizes both graphics together using grid.arrange
#' @export
#'
#' @examples
ggvis_all <- function(scp_output, test_list, 
				   	  c_position = 1:2,
				   	  zoom = 4,
					  train_alpha = .3, 
					  test_color_power = 1/3, 
					  test_color_low = "white",
					  test_color_high = "red"){
	# create data
	data_curves <- data_plot_sc_paths(test_list,scp_output,c_position)

	gg_path <- ggvis_paths(data_curves, zoom = zoom)
	gg_proj <- ggvis_projection(scp_output, train_alpha = train_alpha, 
							 test_color_power = test_color_power, 
							 test_color_low = test_color_low,
							 test_color_high = test_color_high)

	gridExtra::grid.arrange(gg_proj, gg_path, nrow = 1)

	return(list(gg_path = gg_path, gg_proj = gg_proj))

}



