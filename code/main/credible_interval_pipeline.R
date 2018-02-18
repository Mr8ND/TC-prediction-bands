#' Library ----------------------------------------

library(datamart)
library(geosphere)
library(datamart)
library(geosphere)
library(plyr)
library(rworldmap)
library(caret)
library(ks)
require(gtools)


#' Sourcing external functions ----------------------------------------

functions_loc = 'code/functions/'
source(paste0(functions_loc, 'kde_functions.R'))
source(paste0(functions_loc, 'bubble_functions.R'))
source(paste0(functions_loc, 'convex_hull.R'))
source(paste0(functions_loc, 'delta_ball.R'))
source(paste0(functions_loc, 'thirteen_point_compression.R'))
source(paste0(functions_loc, 'path_functions.R'))

#' Functions ---------------------------------------------------------


#' Extract credible intervals of 4 methods from a single test TC
#' 
#' @description
#' This function extracts the KDE, Bubble CI, delta-ball and convex hull 
#' credible intervals from the simulations of a single TC. It then checks the 
#' coverage of such CI as number of points included in the CI for each of the 
#' methods
#' 
#' @param dflist List of simulated TCs
#' @param test_true_path Dataframe with the true TC path to be tested
#' @param alpha_level Alpha level of all the CI
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param unit_measure Unit of measure used for distance
#' @param verbose If TRUE update messages will be displayed
#' 
#' @return 1 list with 4 arguments, once for each of the 4 methods, which all 
#' have an 'area' and 'in_vec' argument to determine the area value and a 
#' boolean vector which reported whether the point of the true TC was inside the
#' TC or not.
credible_interval_single_tc <- function(dflist, test_true_path, alpha_level, 
                                        long = 1, lat = 2, verbose = FALSE,
                                        unit_measure = 'nautical mile'){

    # KDE CI
    kde_ci_list <- kde_from_tclist(dflist = dflist, 
                                    alpha_level = alpha_level)
    kde_predict_mat <- predict_kde_object(kde_obj = kde_ci_list$kde_object, 
                        predict_mat = test_true_path, 
                        alpha_level = alpha_level, 
                        long = long, lat = lat)

    out_kde_list <- list('contour' = kde_ci_list$contour, 
                          'area' = kde_ci_list$area,
                          'in_vec' = kde_predict_mat[, 4])

    # Distance Matrix calculation
    dflist_13pointsreduction <- thirteen_points_listable(dflist, 
                                                    c_position = c(long,lat))
    dist_matrix_13pointsreduction <- distMatrixPath_innersq(
                                                dflist_13pointsreduction
                                                )
    depth_vector <- depth_function(dist_matrix_13pointsreduction)
    depth_vector_idx <- which(depth_vector == max(depth_vector))

    # Bubble CI
    bubble_ci_list <- bubble_ci_from_tclist(dflist = dflist, 
                                      center_idx = depth_vector_idx, 
                                      alpha_level = alpha_level,
                                      long = long, lat = lat, 
                                      unit_measure = unit_measure)
    bubble_ci_inclusion_vec <- check_points_in_bubbleCI(
                                            df_points = test_true_path, 
                                            center_df = bubble_ci_list$centers, 
                                            radius_df = bubble_ci_list$radius, 
                                            long = long, lat = lat)

    out_bubble_list <- list(
                        'bubble_structure' = bubble_ci_list$bubble_CI_object,
                        'area' = bubble_ci_list$area,
                        'area_vector' = bubble_ci_list$area_vector,
                        'in_vec' = bubble_ci_inclusion_vec
                        )
    
    # deep points calculation
    data_deep_points <- depth_curves_to_points(data_list = dflist,
                                    alpha = alpha_level, 
                                    dist_mat = dist_matrix_13pointsreduction, 
                                    c_position = c(long,lat),
                                    depth_vector = depth_vector,
                                    verbose = verbose)

    # Convex Hull CI
    delta_ball_structure <- delta_structure(data_list = dflist, 
                                    alpha = alpha_level, 
                                    dist_mat = dist_matrix_13pointsreduction,
                                    data_deep_points = data_deep_points, 
                                    depth_vector = depth_vector,
                                    c_position = c(long,lat),
                                    area_ci_n = 2000, 
                                    area_ci_alpha = alpha_level, 
                                    verbose = verbose)

    delta_ball_inclusion_vec <- delta_ball_prop_interior(data_deep_points, 
                                                    test_true_path, 
                                                    out_convex_hull_list$delta)

    out_delta_ball_list <- list(
        'structure' = delta_ball_structure$structure,
        'area' = delta_ball_structure$area,
        'area_ci' =  delta_ball_structure$area_ci,
        'delta' = delta_ball_structure$delta,
        'in_vec' = delta_ball_inclusion_vec
        )


    # Delta Ball CI
    convex_hull_structure <- convex_hull_structure(data_list = dflist, 
                                    alpha = alpha_level, 
                                    dist_mat = dist_matrix_13pointsreduction,
                                    data_deep_points = data_deep_points, 
                                    depth_vector = depth_vector,
                                    c_position = c(long,lat),
                                    verbose = verbose)

    convex_hull_inclusion_vec <- points_in_spatial_polygon(
                            convex_hull_structure$spPoly, 
                            as.matrix(test_true_path), 
                            long = long, lat = lat)

    out_convex_hull_list <- list(
        'structure' = convex_hull_structure$poly_df, 
        'area' = convex_hull_structure$area, 
        'spPoly' = convex_hull_structure$spPoly
        'in_vec' = convex_hull_inclusion_vec
        )


    return(list('kde' = out_kde_list, 
                'bubble_ci' = out_bubble_list,
                'delta_ball' = out_delta_ball_list, 
                'convex_hull' = out_convex_hull_list))

}



#' Extract CI from the full list of simulated and true TCs
#' 
#' @description
#' This function iterates through different type of curves and all the simulated curves
#' in order to obtain the credible intervals from the 4 different methods
#' 
#' @details
#' This function is just an iterative wrapper around the function credible_interval_single_tc
#' 
#' @param tc_full_sim_list List of simulated TCs
#' @param tc_true_path_list Dataframe with the true TC path to be tested
#' @param alpha_level Alpha level of all the CI
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param start_idx Starting index when selecting which TC to be simulated - if NULL, defaulted
#' to 1
#' @param end_idx Ending index when selecting which TC to be simulated - if NULL, defaulted to
#' the number of TCs available
#' @param unit_measure Unit of measure used for distance
#' @param verbose if TRUE, an update on the process will be printed
#' @param curve_type_vec vector with the strings referring to the different types of curves
#' 
#' @return A list in which each sublist is a curve type and each sublist has for each element
#' a TC with the 4 different calculated credible intervals.
credible_interval_pipeline <- function(tc_full_sim_list, tc_true_path_list, alpha_level = 0.1,
                                        start_idx = NULL, end_idx = NULL, long = 1, lat = 2, 
                                        unit_measure = 'nautical mile', verbose = TRUE,
                                        curve_type_vec = c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')) {
    output = list()

    # Setting starting and ending index
    if (!is.null(start_idx)){
        start_idx <- 1
    }
    if (!is.null(end_idx)){
        end_idx <- length(tc_true_path_list_curve)
    }

  	if (verbose) {
	    pb <- progress_bar$new(
	      format = "Creating Distance Matrix [:bar] :percent eta: :eta",
	      total = length(c(start_idx:end_idx))*4, clear = FALSE, width = 51)
  	}

    for (curve_type in curve_type_vec) {

        # Retrieving simulated and true curves
        output_curve <- list()
        tc_sim_list_curve <- tc_full_sim_list[[curve_type]]
        tc_true_path_list_curve <- tc_true_path_list[[tc_true_path_list]]

        # Calculating the CI for each of the simulated CI
        for (j in c(start_idx:end_idx)) {
            output_curve[[j]] <- credible_interval_single_tc(dflist = tc_sim_list_curve[[j]], 
                                                              test_true_path = tc_true_path_list_curve[[j]],
                                                              alpha_level = alpha_level,
                                                              long = long, lat = lat, 
                                                              unit_measure = unit_measure,
                                                              verbose = FALSE)
            if (verbose) {
		        pb$tick()
		    }
        }
        output[[curve_type]] <- output_curve
    }
    return(output)
}



#' Execution --------------------------------------------------------

# Data import step as dflist - list of lists of TCs
tc_full_sim_list <- NULL

# Data import step for true TCs DF - test ones
tc_true_path_list <- NULL

alpha_level <- 0.1
output_pipeline <- credible_interval_pipeline(tc_full_sim_list = tc_full_sim_list,
                                                tc_true_path_list = tc_true_path_list,
                                                alpha_level = alpha_level)

out_filename <- paste0('data/output_pipeline_alpha', as.character(alpha_level), '_', Sys.Date(), '.Rdata')
save(output_pipeline, file = out_filename)

